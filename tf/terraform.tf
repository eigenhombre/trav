variable "aws_region" {
  default = "us-east-1"
}

variable "aws_account_id" {}

resource "aws_s3_bucket" "jar_bucket" {
  bucket = "eigenhombre_jars"
  acl    = "private"

  tags {
    Name        = "My bucket"
    Environment = "Dev"
  }
}

resource "aws_s3_bucket_object" "jar_file" {
  bucket = "eigenhombre_jars"
  depends_on = ["aws_s3_bucket.jar_bucket"]
  key    = "trav.jar"
  source = "../target/trav.jar"
  etag   = "${md5(file("../target/trav.jar"))}"
}

resource "aws_iam_role" "iam_for_lambda" {
  name = "iam_for_lambda"

  assume_role_policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "lambda.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
EOF
}

resource "aws_lambda_function" "test_lambda" {
  filename         = "../target/trav.jar"
  function_name    = "Characters"
  role             = "${aws_iam_role.iam_for_lambda.arn}"
  handler          = "trav.lambda.Characters"
  source_code_hash = "${base64sha256(file("../target/trav.jar"))}"
  runtime          = "java8"
  timeout          = 100
  memory_size      = 1024
  environment {
    variables = {
      foo = "bar"
    }
  }
}

resource "aws_lambda_permission" "handler_apigateway_permission" {
  function_name = "${aws_lambda_function.test_lambda.arn}"
  action = "lambda:InvokeFunction"
  statement_id = "AllowExecutionFromApiGateway"
  principal = "apigateway.amazonaws.com"
}

resource "aws_api_gateway_rest_api" "trav_api" {
  name = "TravAPI"
}

resource "aws_api_gateway_resource" "trav_api_res_trav" {
  rest_api_id = "${aws_api_gateway_rest_api.trav_api.id}"
  parent_id   = "${aws_api_gateway_rest_api.trav_api.root_resource_id}"
  path_part   = "trav"
}

resource "aws_api_gateway_rest_api" "proxy" {
  name = "trav-proxy"
}

resource "aws_api_gateway_method" "proxy_root_methods" {
  http_method = "ANY"
  authorization = "NONE"
  rest_api_id = "${aws_api_gateway_rest_api.proxy.id}"
  resource_id = "${aws_api_gateway_rest_api.proxy.root_resource_id}"
}

resource "aws_api_gateway_integration" "proxy_root_handler_integration" {
  type = "AWS_PROXY"
  integration_http_method = "POST"
  rest_api_id = "${aws_api_gateway_rest_api.proxy.id}"
  resource_id = "${aws_api_gateway_rest_api.proxy.root_resource_id}"
  http_method = "${aws_api_gateway_method.proxy_root_methods.http_method}"
  uri = "arn:aws:apigateway:${var.aws_region}:lambda:path/2015-03-31/functions/arn:aws:lambda:${var.aws_region}:${var.aws_account_id}:function:${aws_lambda_function.test_lambda.function_name}/invocations"
}

resource "aws_api_gateway_resource" "proxy_greedy_resource" {
  rest_api_id = "${aws_api_gateway_rest_api.proxy.id}"
  parent_id = "${aws_api_gateway_rest_api.proxy.root_resource_id}"
  path_part = "{proxy+}"
}

resource "aws_api_gateway_method" "proxy_greedy_methods" {
  http_method = "ANY"
  authorization = "NONE"
  rest_api_id = "${aws_api_gateway_rest_api.proxy.id}"
  resource_id = "${aws_api_gateway_resource.proxy_greedy_resource.id}"
}

resource "aws_api_gateway_integration" "proxy_greedy_handler_integration" {
  type = "AWS_PROXY"
  integration_http_method = "POST"
  rest_api_id = "${aws_api_gateway_rest_api.proxy.id}"
  resource_id = "${aws_api_gateway_resource.proxy_greedy_resource.id}"
  http_method = "${aws_api_gateway_method.proxy_greedy_methods.http_method}"
  uri = "arn:aws:apigateway:${var.aws_region}:lambda:path/2015-03-31/functions/arn:aws:lambda:${var.aws_region}:${var.aws_account_id}:function:${aws_lambda_function.test_lambda.function_name}/invocations"
}

resource "aws_api_gateway_deployment" "proxy_deployment" {
  rest_api_id= "${aws_api_gateway_rest_api.proxy.id}"
  stage_name = "production"
  variables = {}
  depends_on = [
    "aws_api_gateway_integration.proxy_root_handler_integration",
    "aws_api_gateway_integration.proxy_greedy_handler_integration",
  ]
}

resource "aws_iam_role" "handler_role" {
  name = "trav-proxy-handler"
  assume_role_policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": [
        "sts:AssumeRole"
      ],
      "Principal": {
        "Service": "lambda.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
  EOF
}

output "url" {
  value = "${aws_api_gateway_deployment.proxy_deployment.invoke_url}"
}
