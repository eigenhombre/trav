variable "AWS_REGION" { default = "us-east-1" }
variable "AWS_ACCOUNT_ID" {}
variable "AWS_ACCESS_KEY" {}
variable "AWS_SECRET_KEY_ID" {}


provider "aws" {
  access_key = "${var.AWS_ACCESS_KEY}"
  secret_key = "${var.AWS_SECRET_KEY_ID}"
  region     = "${var.AWS_REGION}"
}


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
  key    = "trav.zip"
  source = "../trav.zip"
  etag   = "${md5(file("../trav.zip"))}"
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
  filename         = "../trav.zip"
  function_name    = "Characters"
  role             = "${aws_iam_role.iam_for_lambda.arn}"
  handler          = "trav.lambda.Characters"
  source_code_hash = "${base64sha256(file("../trav.zip"))}"
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
  uri = "arn:aws:apigateway:${var.AWS_REGION}:lambda:path/2015-03-31/functions/arn:aws:lambda:${var.AWS_REGION}:${var.AWS_ACCOUNT_ID}:function:${aws_lambda_function.test_lambda.function_name}/invocations"
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
  uri = "arn:aws:apigateway:${var.AWS_REGION}:lambda:path/2015-03-31/functions/arn:aws:lambda:${var.AWS_REGION}:${var.AWS_ACCOUNT_ID}:function:${aws_lambda_function.test_lambda.function_name}/invocations"
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


# Keep Lambda "warm" to make startup much faster:
resource "aws_cloudwatch_event_rule" "trav_5min_rule" {
    name = "trav-every-five-minutes"
    description = "Fires Trav lambda every five minutes"
    schedule_expression = "rate(5 minutes)"
}


resource "aws_lambda_permission" "cw_event_call_trav_lambda" {
    statement_id = "AllowExecutionFromCloudWatch"
    action = "lambda:InvokeFunction"
    function_name = "${aws_lambda_function.test_lambda.function_name}"
    principal = "events.amazonaws.com"
    source_arn = "${aws_cloudwatch_event_rule.trav_5min_rule.arn}"
}


resource "aws_cloudwatch_event_target" "target5min" {
    rule = "${aws_cloudwatch_event_rule.trav_5min_rule.name}"
    target_id = "test_lambda"
    arn = "${aws_lambda_function.test_lambda.arn}"
}


output "url" {
  value = "${aws_api_gateway_deployment.proxy_deployment.invoke_url}"
}
