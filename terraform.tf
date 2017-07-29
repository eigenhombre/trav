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
  key    = "trav.jar"
  source = "target/trav.jar"
  etag   = "${md5(file("target/trav.jar"))}"
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
  filename         = "target/trav.jar"
  function_name    = "Characters"
  role             = "${aws_iam_role.iam_for_lambda.arn}"
  handler          = "trav.lambda.Characters"
  source_code_hash = "${base64sha256(file("target/trav.jar"))}"
  runtime          = "java8"
  timeout          = 100
  environment {
    variables = {
      foo = "bar"
    }
  }
}


# API gateway code adapted from https://github.com/TailorDev/hello-lambda:
resource "aws_api_gateway_rest_api" "trav_api" {
  name = "TravAPI"
}


resource "aws_api_gateway_resource" "trav_api_res_trav" {
  rest_api_id = "${aws_api_gateway_rest_api.trav_api.id}"
  parent_id   = "${aws_api_gateway_rest_api.trav_api.root_resource_id}"
  path_part   = "trav"
}


module "trav_get" {
  source      = "./api_method"
  rest_api_id = "${aws_api_gateway_rest_api.trav_api.id}"
  resource_id = "${aws_api_gateway_resource.trav_api_res_trav.id}"
  method      = "GET"
  path        = "${aws_api_gateway_resource.trav_api_res_trav.path}"
  lambda      = "Characters"
  account_id  = "${var.aws_account_id}"
  region      = "${var.aws_region}"
}


resource "aws_api_gateway_deployment" "trav_api_deployment" {
  rest_api_id = "${aws_api_gateway_rest_api.trav_api.id}"
  stage_name  = "production"
  description = "Deploy method: ${module.trav_get.http_method}"
}

output "url" {
  value = "${aws_api_gateway_deployment.trav_api_deployment.invoke_url}/${aws_api_gateway_resource.trav_api_res_trav.path_part}"
}
