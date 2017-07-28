provider "aws" {
  region     = "us-east-1"
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
  function_name    = "Hello"
  role             = "${aws_iam_role.iam_for_lambda.arn}"
  handler          = "trav.lambda.Hello"
  source_code_hash = "${base64sha256(file("target/trav.jar"))}"
  runtime          = "java8"
  timeout          = 100
  environment {
    variables = {
      foo = "bar"
    }
  }
}