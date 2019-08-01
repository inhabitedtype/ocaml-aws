let endpoint_of svc_name region =
  match svc_name with
  | "a4b" ->
      (match region with
       | "us-east-1" -> Some "a4b.us-east-1.amazonaws.com"
       | _ -> None)
  | "acm" ->
      (match region with
       | "ap-northeast-1" -> Some "acm.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "acm.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "acm.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "acm.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "acm.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "acm.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "acm.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "acm.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "acm.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "acm.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "acm.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "acm.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "acm.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "acm.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "acm.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "acm.us-west-2.amazonaws.com"
       | _ -> None)
  | "acm-pca" ->
      (match region with
       | "ap-northeast-1" -> Some "acm-pca.ap-northeast-1.amazonaws.com"
       | "ap-southeast-1" -> Some "acm-pca.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "acm-pca.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "acm-pca.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "acm-pca.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "acm-pca.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "acm-pca.eu-west-2.amazonaws.com"
       | "us-east-1" -> Some "acm-pca.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "acm-pca.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "acm-pca.us-west-2.amazonaws.com"
       | _ -> None)
  | "api.ecr" ->
      (match region with
       | "ap-northeast-1" -> Some "api.ecr.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "api.ecr.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "api.ecr.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "api.ecr.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "api.ecr.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "api.ecr.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "api.ecr.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "api.ecr.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "api.ecr.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "api.ecr.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "api.ecr.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "api.ecr.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "api.ecr.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "api.ecr.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "api.ecr.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "api.ecr.us-west-2.amazonaws.com"
       | _ -> None)
  | "api.mediatailor" ->
      (match region with
       | "ap-northeast-1" ->
           Some "api.mediatailor.ap-northeast-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "api.mediatailor.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "api.mediatailor.ap-southeast-2.amazonaws.com"
       | "eu-west-1" -> Some "api.mediatailor.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "api.mediatailor.us-east-1.amazonaws.com"
       | "us-west-2" -> Some "api.mediatailor.us-west-2.amazonaws.com"
       | _ -> None)
  | "api.pricing" ->
      (match region with
       | "ap-south-1" -> Some "api.pricing.ap-south-1.amazonaws.com"
       | "us-east-1" -> Some "api.pricing.us-east-1.amazonaws.com"
       | _ -> None)
  | "api.sagemaker" ->
      (match region with
       | "ap-northeast-1" ->
           Some "api.sagemaker.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "api.sagemaker.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "api.sagemaker.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "api.sagemaker.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "api.sagemaker.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "api.sagemaker.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "api.sagemaker.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "api.sagemaker.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "api.sagemaker.eu-west-2.amazonaws.com"
       | "us-east-1" -> Some "api.sagemaker.us-east-1.amazonaws.com"
       | "us-east-1-fips" ->
           Some "api-fips.sagemaker.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "api.sagemaker.us-east-2.amazonaws.com"
       | "us-east-2-fips" ->
           Some "api-fips.sagemaker.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "api.sagemaker.us-west-1.amazonaws.com"
       | "us-west-1-fips" ->
           Some "api-fips.sagemaker.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "api.sagemaker.us-west-2.amazonaws.com"
       | "us-west-2-fips" ->
           Some "api-fips.sagemaker.us-west-2.amazonaws.com"
       | _ -> None)
  | "apigateway" ->
      (match region with
       | "ap-northeast-1" -> Some "apigateway.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "apigateway.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "apigateway.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "apigateway.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "apigateway.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "apigateway.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "apigateway.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "apigateway.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "apigateway.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "apigateway.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "apigateway.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "apigateway.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "apigateway.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "apigateway.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "apigateway.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "apigateway.us-west-2.amazonaws.com"
       | _ -> None)
  | "application-autoscaling" ->
      (match region with
       | "ap-northeast-1" ->
           Some "application-autoscaling.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "application-autoscaling.ap-northeast-2.amazonaws.com"
       | "ap-south-1" ->
           Some "application-autoscaling.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "application-autoscaling.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "application-autoscaling.ap-southeast-2.amazonaws.com"
       | "ca-central-1" ->
           Some "application-autoscaling.ca-central-1.amazonaws.com"
       | "eu-central-1" ->
           Some "application-autoscaling.eu-central-1.amazonaws.com"
       | "eu-north-1" ->
           Some "application-autoscaling.eu-north-1.amazonaws.com"
       | "eu-west-1" ->
           Some "application-autoscaling.eu-west-1.amazonaws.com"
       | "eu-west-2" ->
           Some "application-autoscaling.eu-west-2.amazonaws.com"
       | "eu-west-3" ->
           Some "application-autoscaling.eu-west-3.amazonaws.com"
       | "sa-east-1" ->
           Some "application-autoscaling.sa-east-1.amazonaws.com"
       | "us-east-1" ->
           Some "application-autoscaling.us-east-1.amazonaws.com"
       | "us-east-2" ->
           Some "application-autoscaling.us-east-2.amazonaws.com"
       | "us-west-1" ->
           Some "application-autoscaling.us-west-1.amazonaws.com"
       | "us-west-2" ->
           Some "application-autoscaling.us-west-2.amazonaws.com"
       | _ -> None)
  | "appstream2" ->
      (match region with
       | "ap-northeast-1" -> Some "appstream2.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "appstream2.ap-northeast-2.amazonaws.com"
       | "ap-southeast-1" -> Some "appstream2.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "appstream2.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "appstream2.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "appstream2.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "appstream2.us-east-1.amazonaws.com"
       | "us-west-2" -> Some "appstream2.us-west-2.amazonaws.com"
       | _ -> None)
  | "appsync" ->
      (match region with
       | "ap-northeast-1" -> Some "appsync.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "appsync.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "appsync.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "appsync.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "appsync.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "appsync.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "appsync.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "appsync.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "appsync.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "appsync.us-west-2.amazonaws.com"
       | _ -> None)
  | "athena" ->
      (match region with
       | "ap-northeast-1" -> Some "athena.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "athena.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "athena.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "athena.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "athena.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "athena.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "athena.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "athena.eu-west-2.amazonaws.com"
       | "us-east-1" -> Some "athena.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "athena.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "athena.us-west-2.amazonaws.com"
       | _ -> None)
  | "autoscaling" ->
      (match region with
       | "ap-northeast-1" -> Some "autoscaling.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "autoscaling.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "autoscaling.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "autoscaling.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "autoscaling.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "autoscaling.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "autoscaling.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "autoscaling.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "autoscaling.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "autoscaling.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "autoscaling.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "autoscaling.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "autoscaling.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "autoscaling.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "autoscaling.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "autoscaling.us-west-2.amazonaws.com"
       | _ -> None)
  | "autoscaling-plans" ->
      (match region with
       | "ap-northeast-1" ->
           Some "autoscaling-plans.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "autoscaling-plans.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "autoscaling-plans.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "autoscaling-plans.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "autoscaling-plans.ap-southeast-2.amazonaws.com"
       | "ca-central-1" ->
           Some "autoscaling-plans.ca-central-1.amazonaws.com"
       | "eu-central-1" ->
           Some "autoscaling-plans.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "autoscaling-plans.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "autoscaling-plans.eu-west-2.amazonaws.com"
       | "us-east-1" -> Some "autoscaling-plans.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "autoscaling-plans.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "autoscaling-plans.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "autoscaling-plans.us-west-2.amazonaws.com"
       | _ -> None)
  | "batch" ->
      (match region with
       | "ap-northeast-1" -> Some "batch.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "batch.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "batch.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "batch.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "batch.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "batch.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "batch.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "batch.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "batch.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "batch.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "batch.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "batch.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "batch.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "batch.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "batch.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "batch.us-west-2.amazonaws.com"
       | _ -> None)
  | "budgets" ->
      (match region with
       | "aws-global" -> Some "budgets.amazonaws.com"
       | _ -> None)
  | "ce" ->
      (match region with
       | "aws-global" -> Some "ce.us-east-1.amazonaws.com"
       | _ -> None)
  | "chime" ->
      (match region with
       | "aws-global" -> Some "service.chime.aws.amazon.com"
       | _ -> None)
  | "cloud9" ->
      (match region with
       | "ap-southeast-1" -> Some "cloud9.ap-southeast-1.amazonaws.com"
       | "eu-west-1" -> Some "cloud9.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "cloud9.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "cloud9.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "cloud9.us-west-2.amazonaws.com"
       | _ -> None)
  | "clouddirectory" ->
      (match region with
       | "ap-southeast-1" ->
           Some "clouddirectory.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "clouddirectory.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "clouddirectory.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "clouddirectory.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "clouddirectory.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "clouddirectory.eu-west-2.amazonaws.com"
       | "us-east-1" -> Some "clouddirectory.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "clouddirectory.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "clouddirectory.us-west-2.amazonaws.com"
       | _ -> None)
  | "cloudformation" ->
      (match region with
       | "ap-northeast-1" ->
           Some "cloudformation.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "cloudformation.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "cloudformation.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "cloudformation.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "cloudformation.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "cloudformation.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "cloudformation.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "cloudformation.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "cloudformation.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "cloudformation.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "cloudformation.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "cloudformation.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "cloudformation.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "cloudformation.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "cloudformation.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "cloudformation.us-west-2.amazonaws.com"
       | _ -> None)
  | "cloudfront" ->
      (match region with
       | "aws-global" -> Some "cloudfront.amazonaws.com"
       | _ -> None)
  | "cloudhsm" ->
      (match region with
       | "ap-northeast-1" -> Some "cloudhsm.ap-northeast-1.amazonaws.com"
       | "ap-southeast-1" -> Some "cloudhsm.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "cloudhsm.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "cloudhsm.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "cloudhsm.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "cloudhsm.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "cloudhsm.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "cloudhsm.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "cloudhsm.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "cloudhsm.us-west-2.amazonaws.com"
       | _ -> None)
  | "cloudhsmv2" ->
      (match region with
       | "ap-northeast-1" -> Some "cloudhsmv2.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "cloudhsmv2.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "cloudhsmv2.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "cloudhsmv2.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "cloudhsmv2.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "cloudhsmv2.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "cloudhsmv2.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "cloudhsmv2.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "cloudhsmv2.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "cloudhsmv2.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "cloudhsmv2.eu-west-3.amazonaws.com"
       | "us-east-1" -> Some "cloudhsmv2.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "cloudhsmv2.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "cloudhsmv2.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "cloudhsmv2.us-west-2.amazonaws.com"
       | _ -> None)
  | "cloudsearch" ->
      (match region with
       | "ap-northeast-1" -> Some "cloudsearch.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "cloudsearch.ap-northeast-2.amazonaws.com"
       | "ap-southeast-1" -> Some "cloudsearch.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "cloudsearch.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "cloudsearch.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "cloudsearch.eu-west-1.amazonaws.com"
       | "sa-east-1" -> Some "cloudsearch.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "cloudsearch.us-east-1.amazonaws.com"
       | "us-west-1" -> Some "cloudsearch.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "cloudsearch.us-west-2.amazonaws.com"
       | _ -> None)
  | "cloudtrail" ->
      (match region with
       | "ap-northeast-1" -> Some "cloudtrail.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "cloudtrail.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "cloudtrail.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "cloudtrail.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "cloudtrail.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "cloudtrail.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "cloudtrail.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "cloudtrail.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "cloudtrail.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "cloudtrail.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "cloudtrail.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "cloudtrail.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "cloudtrail.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "cloudtrail.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "cloudtrail.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "cloudtrail.us-west-2.amazonaws.com"
       | _ -> None)
  | "codebuild" ->
      (match region with
       | "ap-northeast-1" -> Some "codebuild.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "codebuild.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "codebuild.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "codebuild.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "codebuild.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "codebuild.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "codebuild.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "codebuild.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "codebuild.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "codebuild.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "codebuild.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "codebuild.us-east-1.amazonaws.com"
       | "us-east-1-fips" -> Some "codebuild-fips.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "codebuild.us-east-2.amazonaws.com"
       | "us-east-2-fips" -> Some "codebuild-fips.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "codebuild.us-west-1.amazonaws.com"
       | "us-west-1-fips" -> Some "codebuild-fips.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "codebuild.us-west-2.amazonaws.com"
       | "us-west-2-fips" -> Some "codebuild-fips.us-west-2.amazonaws.com"
       | _ -> None)
  | "codecommit" ->
      (match region with
       | "ap-northeast-1" -> Some "codecommit.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "codecommit.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "codecommit.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "codecommit.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "codecommit.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "codecommit.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "codecommit.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "codecommit.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "codecommit.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "codecommit.eu-west-3.amazonaws.com"
       | "fips" -> Some "codecommit-fips.ca-central-1.amazonaws.com"
       | "sa-east-1" -> Some "codecommit.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "codecommit.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "codecommit.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "codecommit.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "codecommit.us-west-2.amazonaws.com"
       | _ -> None)
  | "codedeploy" ->
      (match region with
       | "ap-northeast-1" -> Some "codedeploy.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "codedeploy.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "codedeploy.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "codedeploy.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "codedeploy.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "codedeploy.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "codedeploy.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "codedeploy.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "codedeploy.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "codedeploy.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "codedeploy.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "codedeploy.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "codedeploy.us-east-1.amazonaws.com"
       | "us-east-1-fips" -> Some "codedeploy-fips.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "codedeploy.us-east-2.amazonaws.com"
       | "us-east-2-fips" -> Some "codedeploy-fips.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "codedeploy.us-west-1.amazonaws.com"
       | "us-west-1-fips" -> Some "codedeploy-fips.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "codedeploy.us-west-2.amazonaws.com"
       | "us-west-2-fips" -> Some "codedeploy-fips.us-west-2.amazonaws.com"
       | _ -> None)
  | "codepipeline" ->
      (match region with
       | "ap-northeast-1" -> Some "codepipeline.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "codepipeline.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "codepipeline.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "codepipeline.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "codepipeline.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "codepipeline.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "codepipeline.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "codepipeline.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "codepipeline.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "codepipeline.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "codepipeline.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "codepipeline.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "codepipeline.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "codepipeline.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "codepipeline.us-west-2.amazonaws.com"
       | _ -> None)
  | "codestar" ->
      (match region with
       | "ap-northeast-1" -> Some "codestar.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "codestar.ap-northeast-2.amazonaws.com"
       | "ap-southeast-1" -> Some "codestar.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "codestar.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "codestar.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "codestar.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "codestar.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "codestar.eu-west-2.amazonaws.com"
       | "us-east-1" -> Some "codestar.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "codestar.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "codestar.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "codestar.us-west-2.amazonaws.com"
       | _ -> None)
  | "cognito-identity" ->
      (match region with
       | "ap-northeast-1" ->
           Some "cognito-identity.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "cognito-identity.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "cognito-identity.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "cognito-identity.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "cognito-identity.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "cognito-identity.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "cognito-identity.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "cognito-identity.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "cognito-identity.eu-west-2.amazonaws.com"
       | "us-east-1" -> Some "cognito-identity.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "cognito-identity.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "cognito-identity.us-west-2.amazonaws.com"
       | _ -> None)
  | "cognito-idp" ->
      (match region with
       | "ap-northeast-1" -> Some "cognito-idp.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "cognito-idp.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "cognito-idp.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "cognito-idp.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "cognito-idp.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "cognito-idp.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "cognito-idp.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "cognito-idp.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "cognito-idp.eu-west-2.amazonaws.com"
       | "us-east-1" -> Some "cognito-idp.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "cognito-idp.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "cognito-idp.us-west-2.amazonaws.com"
       | _ -> None)
  | "cognito-sync" ->
      (match region with
       | "ap-northeast-1" -> Some "cognito-sync.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "cognito-sync.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "cognito-sync.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "cognito-sync.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "cognito-sync.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "cognito-sync.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "cognito-sync.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "cognito-sync.eu-west-2.amazonaws.com"
       | "us-east-1" -> Some "cognito-sync.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "cognito-sync.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "cognito-sync.us-west-2.amazonaws.com"
       | _ -> None)
  | "comprehend" ->
      (match region with
       | "ap-southeast-1" -> Some "comprehend.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "comprehend.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "comprehend.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "comprehend.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "comprehend.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "comprehend.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "comprehend.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "comprehend.us-west-2.amazonaws.com"
       | _ -> None)
  | "config" ->
      (match region with
       | "ap-northeast-1" -> Some "config.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "config.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "config.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "config.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "config.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "config.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "config.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "config.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "config.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "config.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "config.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "config.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "config.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "config.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "config.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "config.us-west-2.amazonaws.com"
       | _ -> None)
  | "cur" ->
      (match region with
       | "us-east-1" -> Some "cur.us-east-1.amazonaws.com"
       | _ -> None)
  | "data.iot" ->
      (match region with
       | "ap-northeast-1" -> Some "data.iot.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "data.iot.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "data.iot.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "data.iot.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "data.iot.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "data.iot.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "data.iot.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "data.iot.eu-west-2.amazonaws.com"
       | "us-east-1" -> Some "data.iot.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "data.iot.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "data.iot.us-west-2.amazonaws.com"
       | _ -> None)
  | "datapipeline" ->
      (match region with
       | "ap-northeast-1" -> Some "datapipeline.ap-northeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "datapipeline.ap-southeast-2.amazonaws.com"
       | "eu-west-1" -> Some "datapipeline.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "datapipeline.us-east-1.amazonaws.com"
       | "us-west-2" -> Some "datapipeline.us-west-2.amazonaws.com"
       | _ -> None)
  | "datasync" ->
      (match region with
       | "ap-northeast-1" -> Some "datasync.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "datasync.ap-northeast-2.amazonaws.com"
       | "ap-southeast-1" -> Some "datasync.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "datasync.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "datasync.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "datasync.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "datasync.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "datasync.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "datasync.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "datasync.us-west-2.amazonaws.com"
       | _ -> None)
  | "dax" ->
      (match region with
       | "ap-northeast-1" -> Some "dax.ap-northeast-1.amazonaws.com"
       | "ap-south-1" -> Some "dax.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "dax.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "dax.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "dax.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "dax.eu-west-1.amazonaws.com"
       | "sa-east-1" -> Some "dax.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "dax.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "dax.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "dax.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "dax.us-west-2.amazonaws.com"
       | _ -> None)
  | "devicefarm" ->
      (match region with
       | "us-west-2" -> Some "devicefarm.us-west-2.amazonaws.com"
       | _ -> None)
  | "directconnect" ->
      (match region with
       | "ap-northeast-1" ->
           Some "directconnect.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "directconnect.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "directconnect.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "directconnect.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "directconnect.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "directconnect.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "directconnect.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "directconnect.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "directconnect.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "directconnect.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "directconnect.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "directconnect.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "directconnect.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "directconnect.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "directconnect.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "directconnect.us-west-2.amazonaws.com"
       | _ -> None)
  | "discovery" ->
      (match region with
       | "us-west-2" -> Some "discovery.us-west-2.amazonaws.com"
       | _ -> None)
  | "dms" ->
      (match region with
       | "ap-northeast-1" -> Some "dms.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "dms.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "dms.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "dms.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "dms.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "dms.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "dms.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "dms.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "dms.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "dms.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "dms.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "dms.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "dms.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "dms.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "dms.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "dms.us-west-2.amazonaws.com"
       | _ -> None)
  | "docdb" ->
      (match region with
       | "eu-west-1" -> Some "rds.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "rds.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "rds.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "rds.us-west-2.amazonaws.com"
       | _ -> None)
  | "ds" ->
      (match region with
       | "ap-northeast-1" -> Some "ds.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "ds.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "ds.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "ds.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "ds.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "ds.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "ds.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "ds.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "ds.eu-west-2.amazonaws.com"
       | "sa-east-1" -> Some "ds.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "ds.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "ds.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "ds.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "ds.us-west-2.amazonaws.com"
       | _ -> None)
  | "dynamodb" ->
      (match region with
       | "ap-northeast-1" -> Some "dynamodb.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "dynamodb.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "dynamodb.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "dynamodb.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "dynamodb.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "dynamodb.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "dynamodb.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "dynamodb.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "dynamodb.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "dynamodb.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "dynamodb.eu-west-3.amazonaws.com"
       | "local" -> Some "localhost:8000"
       | "sa-east-1" -> Some "dynamodb.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "dynamodb.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "dynamodb.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "dynamodb.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "dynamodb.us-west-2.amazonaws.com"
       | _ -> None)
  | "ec2" ->
      (match region with
       | "ap-northeast-1" -> Some "ec2.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "ec2.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "ec2.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "ec2.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "ec2.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "ec2.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "ec2.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "ec2.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "ec2.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "ec2.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "ec2.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "ec2.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "ec2.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "ec2.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "ec2.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "ec2.us-west-2.amazonaws.com"
       | _ -> None)
  | "ecs" ->
      (match region with
       | "ap-northeast-1" -> Some "ecs.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "ecs.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "ecs.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "ecs.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "ecs.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "ecs.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "ecs.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "ecs.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "ecs.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "ecs.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "ecs.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "ecs.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "ecs.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "ecs.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "ecs.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "ecs.us-west-2.amazonaws.com"
       | _ -> None)
  | "elasticache" ->
      (match region with
       | "ap-northeast-1" -> Some "elasticache.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "elasticache.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "elasticache.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "elasticache.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "elasticache.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "elasticache.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "elasticache.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "elasticache.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "elasticache.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "elasticache.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "elasticache.eu-west-3.amazonaws.com"
       | "fips" -> Some "elasticache-fips.us-west-1.amazonaws.com"
       | "sa-east-1" -> Some "elasticache.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "elasticache.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "elasticache.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "elasticache.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "elasticache.us-west-2.amazonaws.com"
       | _ -> None)
  | "elasticbeanstalk" ->
      (match region with
       | "ap-northeast-1" ->
           Some "elasticbeanstalk.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "elasticbeanstalk.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "elasticbeanstalk.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "elasticbeanstalk.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "elasticbeanstalk.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "elasticbeanstalk.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "elasticbeanstalk.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "elasticbeanstalk.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "elasticbeanstalk.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "elasticbeanstalk.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "elasticbeanstalk.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "elasticbeanstalk.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "elasticbeanstalk.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "elasticbeanstalk.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "elasticbeanstalk.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "elasticbeanstalk.us-west-2.amazonaws.com"
       | _ -> None)
  | "elasticfilesystem" ->
      (match region with
       | "ap-northeast-1" ->
           Some "elasticfilesystem.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "elasticfilesystem.ap-northeast-2.amazonaws.com"
       | "ap-southeast-1" ->
           Some "elasticfilesystem.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "elasticfilesystem.ap-southeast-2.amazonaws.com"
       | "eu-central-1" ->
           Some "elasticfilesystem.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "elasticfilesystem.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "elasticfilesystem.eu-west-2.amazonaws.com"
       | "us-east-1" -> Some "elasticfilesystem.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "elasticfilesystem.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "elasticfilesystem.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "elasticfilesystem.us-west-2.amazonaws.com"
       | _ -> None)
  | "elasticloadbalancing" ->
      (match region with
       | "ap-northeast-1" ->
           Some "elasticloadbalancing.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "elasticloadbalancing.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "elasticloadbalancing.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "elasticloadbalancing.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "elasticloadbalancing.ap-southeast-2.amazonaws.com"
       | "ca-central-1" ->
           Some "elasticloadbalancing.ca-central-1.amazonaws.com"
       | "eu-central-1" ->
           Some "elasticloadbalancing.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "elasticloadbalancing.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "elasticloadbalancing.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "elasticloadbalancing.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "elasticloadbalancing.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "elasticloadbalancing.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "elasticloadbalancing.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "elasticloadbalancing.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "elasticloadbalancing.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "elasticloadbalancing.us-west-2.amazonaws.com"
       | _ -> None)
  | "elasticmapreduce" ->
      (match region with
       | "ap-northeast-1" ->
           Some "elasticmapreduce.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "elasticmapreduce.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "elasticmapreduce.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "elasticmapreduce.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "elasticmapreduce.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "elasticmapreduce.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "elasticmapreduce.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "elasticmapreduce.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "elasticmapreduce.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "elasticmapreduce.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "elasticmapreduce.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "elasticmapreduce.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "elasticmapreduce.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "elasticmapreduce.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "elasticmapreduce.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "elasticmapreduce.us-west-2.amazonaws.com"
       | _ -> None)
  | "elastictranscoder" ->
      (match region with
       | "ap-northeast-1" ->
           Some "elastictranscoder.ap-northeast-1.amazonaws.com"
       | "ap-south-1" -> Some "elastictranscoder.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "elastictranscoder.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "elastictranscoder.ap-southeast-2.amazonaws.com"
       | "eu-west-1" -> Some "elastictranscoder.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "elastictranscoder.us-east-1.amazonaws.com"
       | "us-west-1" -> Some "elastictranscoder.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "elastictranscoder.us-west-2.amazonaws.com"
       | _ -> None)
  | "email" ->
      (match region with
       | "eu-west-1" -> Some "email.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "email.us-east-1.amazonaws.com"
       | "us-west-2" -> Some "email.us-west-2.amazonaws.com"
       | _ -> None)
  | "entitlement.marketplace" ->
      (match region with
       | "us-east-1" ->
           Some "entitlement.marketplace.us-east-1.amazonaws.com"
       | _ -> None)
  | "es" ->
      (match region with
       | "ap-northeast-1" -> Some "es.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "es.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "es.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "es.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "es.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "es.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "es.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "es.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "es.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "es.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "es.eu-west-3.amazonaws.com"
       | "fips" -> Some "es-fips.us-west-1.amazonaws.com"
       | "sa-east-1" -> Some "es.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "es.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "es.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "es.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "es.us-west-2.amazonaws.com"
       | _ -> None)
  | "events" ->
      (match region with
       | "ap-northeast-1" -> Some "events.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "events.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "events.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "events.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "events.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "events.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "events.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "events.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "events.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "events.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "events.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "events.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "events.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "events.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "events.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "events.us-west-2.amazonaws.com"
       | _ -> None)
  | "firehose" ->
      (match region with
       | "ap-northeast-1" -> Some "firehose.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "firehose.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "firehose.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "firehose.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "firehose.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "firehose.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "firehose.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "firehose.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "firehose.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "firehose.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "firehose.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "firehose.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "firehose.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "firehose.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "firehose.us-west-2.amazonaws.com"
       | _ -> None)
  | "fms" ->
      (match region with
       | "ap-northeast-1" -> Some "fms.ap-northeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "fms.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "fms.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "fms.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "fms.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "fms.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "fms.us-west-2.amazonaws.com"
       | _ -> None)
  | "fsx" ->
      (match region with
       | "eu-west-1" -> Some "fsx.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "fsx.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "fsx.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "fsx.us-west-2.amazonaws.com"
       | _ -> None)
  | "gamelift" ->
      (match region with
       | "ap-northeast-1" -> Some "gamelift.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "gamelift.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "gamelift.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "gamelift.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "gamelift.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "gamelift.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "gamelift.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "gamelift.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "gamelift.eu-west-2.amazonaws.com"
       | "sa-east-1" -> Some "gamelift.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "gamelift.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "gamelift.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "gamelift.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "gamelift.us-west-2.amazonaws.com"
       | _ -> None)
  | "glacier" ->
      (match region with
       | "ap-northeast-1" -> Some "glacier.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "glacier.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "glacier.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "glacier.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "glacier.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "glacier.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "glacier.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "glacier.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "glacier.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "glacier.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "glacier.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "glacier.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "glacier.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "glacier.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "glacier.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "glacier.us-west-2.amazonaws.com"
       | _ -> None)
  | "glue" ->
      (match region with
       | "ap-northeast-1" -> Some "glue.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "glue.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "glue.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "glue.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "glue.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "glue.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "glue.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "glue.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "glue.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "glue.eu-west-3.amazonaws.com"
       | "us-east-1" -> Some "glue.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "glue.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "glue.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "glue.us-west-2.amazonaws.com"
       | _ -> None)
  | "greengrass" ->
      (match region with
       | "ap-northeast-1" -> Some "greengrass.ap-northeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "greengrass.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "greengrass.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "greengrass.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "greengrass.us-east-1.amazonaws.com"
       | "us-west-2" -> Some "greengrass.us-west-2.amazonaws.com"
       | _ -> None)
  | "guardduty" ->
      (match region with
       | "ap-northeast-1" -> Some "guardduty.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "guardduty.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "guardduty.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "guardduty.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "guardduty.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "guardduty.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "guardduty.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "guardduty.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "guardduty.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "guardduty.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "guardduty.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "guardduty.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "guardduty.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "guardduty.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "guardduty.us-west-2.amazonaws.com"
       | _ -> None)
  | "health" ->
      (match region with
       | "us-east-1" -> Some "health.us-east-1.amazonaws.com"
       | _ -> None)
  | "iam" ->
      (match region with
       | "aws-global" -> Some "iam.amazonaws.com"
       | _ -> None)
  | "importexport" ->
      (match region with
       | "aws-global" -> Some "importexport.amazonaws.com"
       | _ -> None)
  | "inspector" ->
      (match region with
       | "ap-northeast-1" -> Some "inspector.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "inspector.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "inspector.ap-south-1.amazonaws.com"
       | "ap-southeast-2" -> Some "inspector.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "inspector.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "inspector.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "inspector.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "inspector.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "inspector.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "inspector.us-west-2.amazonaws.com"
       | _ -> None)
  | "iot" ->
      (match region with
       | "ap-northeast-1" -> Some "iot.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "iot.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "iot.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "iot.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "iot.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "iot.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "iot.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "iot.eu-west-2.amazonaws.com"
       | "us-east-1" -> Some "iot.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "iot.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "iot.us-west-2.amazonaws.com"
       | _ -> None)
  | "iotanalytics" ->
      (match region with
       | "ap-northeast-1" -> Some "iotanalytics.ap-northeast-1.amazonaws.com"
       | "eu-central-1" -> Some "iotanalytics.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "iotanalytics.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "iotanalytics.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "iotanalytics.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "iotanalytics.us-west-2.amazonaws.com"
       | _ -> None)
  | "kinesis" ->
      (match region with
       | "ap-northeast-1" -> Some "kinesis.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "kinesis.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "kinesis.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "kinesis.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "kinesis.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "kinesis.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "kinesis.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "kinesis.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "kinesis.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "kinesis.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "kinesis.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "kinesis.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "kinesis.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "kinesis.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "kinesis.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "kinesis.us-west-2.amazonaws.com"
       | _ -> None)
  | "kinesisanalytics" ->
      (match region with
       | "eu-central-1" -> Some "kinesisanalytics.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "kinesisanalytics.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "kinesisanalytics.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "kinesisanalytics.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "kinesisanalytics.us-west-2.amazonaws.com"
       | _ -> None)
  | "kinesisvideo" ->
      (match region with
       | "ap-northeast-1" -> Some "kinesisvideo.ap-northeast-1.amazonaws.com"
       | "eu-central-1" -> Some "kinesisvideo.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "kinesisvideo.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "kinesisvideo.us-east-1.amazonaws.com"
       | "us-west-2" -> Some "kinesisvideo.us-west-2.amazonaws.com"
       | _ -> None)
  | "kms" ->
      (match region with
       | "ProdFips" -> Some "kms-fips.ca-central-1.amazonaws.com"
       | "ap-northeast-1" -> Some "kms.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "kms.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "kms.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "kms.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "kms.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "kms.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "kms.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "kms.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "kms.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "kms.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "kms.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "kms.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "kms.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "kms.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "kms.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "kms.us-west-2.amazonaws.com"
       | _ -> None)
  | "lambda" ->
      (match region with
       | "ap-northeast-1" -> Some "lambda.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "lambda.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "lambda.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "lambda.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "lambda.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "lambda.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "lambda.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "lambda.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "lambda.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "lambda.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "lambda.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "lambda.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "lambda.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "lambda.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "lambda.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "lambda.us-west-2.amazonaws.com"
       | _ -> None)
  | "license-manager" ->
      (match region with
       | "ap-northeast-1" ->
           Some "license-manager.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "license-manager.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "license-manager.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "license-manager.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "license-manager.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "license-manager.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "license-manager.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "license-manager.eu-west-2.amazonaws.com"
       | "us-east-1" -> Some "license-manager.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "license-manager.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "license-manager.us-west-2.amazonaws.com"
       | _ -> None)
  | "lightsail" ->
      (match region with
       | "ap-northeast-1" -> Some "lightsail.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "lightsail.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "lightsail.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "lightsail.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "lightsail.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "lightsail.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "lightsail.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "lightsail.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "lightsail.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "lightsail.eu-west-3.amazonaws.com"
       | "us-east-1" -> Some "lightsail.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "lightsail.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "lightsail.us-west-2.amazonaws.com"
       | _ -> None)
  | "logs" ->
      (match region with
       | "ap-northeast-1" -> Some "logs.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "logs.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "logs.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "logs.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "logs.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "logs.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "logs.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "logs.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "logs.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "logs.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "logs.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "logs.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "logs.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "logs.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "logs.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "logs.us-west-2.amazonaws.com"
       | _ -> None)
  | "machinelearning" ->
      (match region with
       | "eu-west-1" -> Some "machinelearning.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "machinelearning.us-east-1.amazonaws.com"
       | _ -> None)
  | "marketplacecommerceanalytics" ->
      (match region with
       | "us-east-1" ->
           Some "marketplacecommerceanalytics.us-east-1.amazonaws.com"
       | _ -> None)
  | "mediaconvert" ->
      (match region with
       | "ap-northeast-1" -> Some "mediaconvert.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "mediaconvert.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "mediaconvert.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "mediaconvert.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "mediaconvert.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "mediaconvert.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "mediaconvert.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "mediaconvert.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "mediaconvert.eu-west-2.amazonaws.com"
       | "sa-east-1" -> Some "mediaconvert.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "mediaconvert.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "mediaconvert.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "mediaconvert.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "mediaconvert.us-west-2.amazonaws.com"
       | _ -> None)
  | "medialive" ->
      (match region with
       | "ap-northeast-1" -> Some "medialive.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "medialive.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "medialive.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "medialive.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "medialive.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "medialive.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "medialive.eu-west-1.amazonaws.com"
       | "sa-east-1" -> Some "medialive.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "medialive.us-east-1.amazonaws.com"
       | "us-west-2" -> Some "medialive.us-west-2.amazonaws.com"
       | _ -> None)
  | "mediapackage" ->
      (match region with
       | "ap-northeast-1" -> Some "mediapackage.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "mediapackage.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "mediapackage.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "mediapackage.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "mediapackage.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "mediapackage.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "mediapackage.eu-west-1.amazonaws.com"
       | "eu-west-3" -> Some "mediapackage.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "mediapackage.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "mediapackage.us-east-1.amazonaws.com"
       | "us-west-1" -> Some "mediapackage.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "mediapackage.us-west-2.amazonaws.com"
       | _ -> None)
  | "mediastore" ->
      (match region with
       | "ap-northeast-1" -> Some "mediastore.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "mediastore.ap-northeast-2.amazonaws.com"
       | "ap-southeast-2" -> Some "mediastore.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "mediastore.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "mediastore.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "mediastore.us-east-1.amazonaws.com"
       | "us-west-2" -> Some "mediastore.us-west-2.amazonaws.com"
       | _ -> None)
  | "metering.marketplace" ->
      (match region with
       | "ap-northeast-1" ->
           Some "metering.marketplace.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "metering.marketplace.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "metering.marketplace.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "metering.marketplace.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "metering.marketplace.ap-southeast-2.amazonaws.com"
       | "ca-central-1" ->
           Some "metering.marketplace.ca-central-1.amazonaws.com"
       | "eu-central-1" ->
           Some "metering.marketplace.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "metering.marketplace.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "metering.marketplace.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "metering.marketplace.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "metering.marketplace.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "metering.marketplace.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "metering.marketplace.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "metering.marketplace.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "metering.marketplace.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "metering.marketplace.us-west-2.amazonaws.com"
       | _ -> None)
  | "mgh" ->
      (match region with
       | "us-west-2" -> Some "mgh.us-west-2.amazonaws.com"
       | _ -> None)
  | "mobileanalytics" ->
      (match region with
       | "us-east-1" -> Some "mobileanalytics.us-east-1.amazonaws.com"
       | _ -> None)
  | "models.lex" ->
      (match region with
       | "eu-west-1" -> Some "models.lex.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "models.lex.us-east-1.amazonaws.com"
       | "us-west-2" -> Some "models.lex.us-west-2.amazonaws.com"
       | _ -> None)
  | "monitoring" ->
      (match region with
       | "ap-northeast-1" -> Some "monitoring.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "monitoring.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "monitoring.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "monitoring.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "monitoring.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "monitoring.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "monitoring.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "monitoring.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "monitoring.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "monitoring.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "monitoring.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "monitoring.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "monitoring.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "monitoring.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "monitoring.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "monitoring.us-west-2.amazonaws.com"
       | _ -> None)
  | "mq" ->
      (match region with
       | "ap-northeast-1" -> Some "mq.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "mq.ap-northeast-2.amazonaws.com"
       | "ap-southeast-1" -> Some "mq.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "mq.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "mq.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "mq.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "mq.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "mq.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "mq.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "mq.us-west-2.amazonaws.com"
       | _ -> None)
  | "mturk-requester" ->
      (match region with
       | "sandbox" -> Some "mturk-requester-sandbox.us-east-1.amazonaws.com"
       | "us-east-1" -> Some "mturk-requester.us-east-1.amazonaws.com"
       | _ -> None)
  | "neptune" ->
      (match region with
       | "ap-northeast-1" -> Some "rds.ap-northeast-1.amazonaws.com"
       | "ap-southeast-1" -> Some "rds.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "rds.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "rds.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "rds.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "rds.eu-west-2.amazonaws.com"
       | "us-east-1" -> Some "rds.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "rds.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "rds.us-west-2.amazonaws.com"
       | _ -> None)
  | "opsworks" ->
      (match region with
       | "ap-northeast-1" -> Some "opsworks.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "opsworks.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "opsworks.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "opsworks.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "opsworks.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "opsworks.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "opsworks.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "opsworks.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "opsworks.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "opsworks.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "opsworks.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "opsworks.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "opsworks.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "opsworks.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "opsworks.us-west-2.amazonaws.com"
       | _ -> None)
  | "opsworks-cm" ->
      (match region with
       | "ap-northeast-1" -> Some "opsworks-cm.ap-northeast-1.amazonaws.com"
       | "ap-southeast-1" -> Some "opsworks-cm.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "opsworks-cm.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "opsworks-cm.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "opsworks-cm.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "opsworks-cm.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "opsworks-cm.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "opsworks-cm.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "opsworks-cm.us-west-2.amazonaws.com"
       | _ -> None)
  | "organizations" ->
      (match region with
       | "aws-global" -> Some "organizations.us-east-1.amazonaws.com"
       | _ -> None)
  | "pinpoint" ->
      (match region with
       | "eu-central-1" -> Some "pinpoint.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "pinpoint.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "pinpoint.us-east-1.amazonaws.com"
       | "us-west-2" -> Some "pinpoint.us-west-2.amazonaws.com"
       | _ -> None)
  | "polly" ->
      (match region with
       | "ap-northeast-1" -> Some "polly.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "polly.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "polly.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "polly.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "polly.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "polly.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "polly.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "polly.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "polly.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "polly.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "polly.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "polly.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "polly.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "polly.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "polly.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "polly.us-west-2.amazonaws.com"
       | _ -> None)
  | "rds" ->
      (match region with
       | "ap-northeast-1" -> Some "rds.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "rds.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "rds.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "rds.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "rds.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "rds.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "rds.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "rds.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "rds.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "rds.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "rds.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "rds.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "rds.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "rds.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "rds.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "rds.us-west-2.amazonaws.com"
       | _ -> None)
  | "redshift" ->
      (match region with
       | "ap-northeast-1" -> Some "redshift.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "redshift.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "redshift.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "redshift.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "redshift.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "redshift.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "redshift.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "redshift.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "redshift.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "redshift.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "redshift.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "redshift.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "redshift.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "redshift.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "redshift.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "redshift.us-west-2.amazonaws.com"
       | _ -> None)
  | "rekognition" ->
      (match region with
       | "ap-northeast-1" -> Some "rekognition.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "rekognition.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "rekognition.ap-south-1.amazonaws.com"
       | "ap-southeast-2" -> Some "rekognition.ap-southeast-2.amazonaws.com"
       | "eu-west-1" -> Some "rekognition.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "rekognition.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "rekognition.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "rekognition.us-west-2.amazonaws.com"
       | _ -> None)
  | "resource-groups" ->
      (match region with
       | "ap-northeast-1" ->
           Some "resource-groups.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "resource-groups.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "resource-groups.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "resource-groups.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "resource-groups.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "resource-groups.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "resource-groups.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "resource-groups.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "resource-groups.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "resource-groups.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "resource-groups.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "resource-groups.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "resource-groups.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "resource-groups.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "resource-groups.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "resource-groups.us-west-2.amazonaws.com"
       | _ -> None)
  | "robomaker" ->
      (match region with
       | "eu-west-1" -> Some "robomaker.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "robomaker.us-east-1.amazonaws.com"
       | "us-west-2" -> Some "robomaker.us-west-2.amazonaws.com"
       | _ -> None)
  | "route53" ->
      (match region with
       | "aws-global" -> Some "route53.amazonaws.com"
       | _ -> None)
  | "route53domains" ->
      (match region with
       | "us-east-1" -> Some "route53domains.us-east-1.amazonaws.com"
       | _ -> None)
  | "route53resolver" ->
      (match region with
       | "ap-northeast-1" ->
           Some "route53resolver.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "route53resolver.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "route53resolver.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "route53resolver.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "route53resolver.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "route53resolver.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "route53resolver.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "route53resolver.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "route53resolver.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "route53resolver.eu-west-3.amazonaws.com"
       | "us-east-1" -> Some "route53resolver.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "route53resolver.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "route53resolver.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "route53resolver.us-west-2.amazonaws.com"
       | _ -> None)
  | "runtime.lex" ->
      (match region with
       | "eu-west-1" -> Some "runtime.lex.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "runtime.lex.us-east-1.amazonaws.com"
       | "us-west-2" -> Some "runtime.lex.us-west-2.amazonaws.com"
       | _ -> None)
  | "runtime.sagemaker" ->
      (match region with
       | "ap-northeast-1" ->
           Some "runtime.sagemaker.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "runtime.sagemaker.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "runtime.sagemaker.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "runtime.sagemaker.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "runtime.sagemaker.ap-southeast-2.amazonaws.com"
       | "ca-central-1" ->
           Some "runtime.sagemaker.ca-central-1.amazonaws.com"
       | "eu-central-1" ->
           Some "runtime.sagemaker.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "runtime.sagemaker.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "runtime.sagemaker.eu-west-2.amazonaws.com"
       | "us-east-1" -> Some "runtime.sagemaker.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "runtime.sagemaker.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "runtime.sagemaker.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "runtime.sagemaker.us-west-2.amazonaws.com"
       | _ -> None)
  | "s3" ->
      (match region with
       | "ap-northeast-1" -> Some "s3.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "s3.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "s3.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "s3.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "s3.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "s3.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "s3.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "s3.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "s3.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "s3.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "s3.eu-west-3.amazonaws.com"
       | "s3-external-1" -> Some "s3-external-1.amazonaws.com"
       | "sa-east-1" -> Some "s3.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "s3.amazonaws.com"
       | "us-east-2" -> Some "s3.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "s3.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "s3.us-west-2.amazonaws.com"
       | _ -> None)
  | "s3-control" ->
      (match region with
       | "ap-northeast-1" -> Some "s3-control.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "s3-control.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "s3-control.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "s3-control.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "s3-control.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "s3-control.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "s3-control.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "s3-control.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "s3-control.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "s3-control.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "s3-control.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "s3-control.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "s3-control.us-east-1.amazonaws.com"
       | "us-east-1-fips" -> Some "s3-control-fips.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "s3-control.us-east-2.amazonaws.com"
       | "us-east-2-fips" -> Some "s3-control-fips.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "s3-control.us-west-1.amazonaws.com"
       | "us-west-1-fips" -> Some "s3-control-fips.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "s3-control.us-west-2.amazonaws.com"
       | "us-west-2-fips" -> Some "s3-control-fips.us-west-2.amazonaws.com"
       | _ -> None)
  | "sdb" ->
      (match region with
       | "ap-northeast-1" -> Some "sdb.ap-northeast-1.amazonaws.com"
       | "ap-southeast-1" -> Some "sdb.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "sdb.ap-southeast-2.amazonaws.com"
       | "eu-west-1" -> Some "sdb.eu-west-1.amazonaws.com"
       | "sa-east-1" -> Some "sdb.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "sdb.amazonaws.com"
       | "us-west-1" -> Some "sdb.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "sdb.us-west-2.amazonaws.com"
       | _ -> None)
  | "secretsmanager" ->
      (match region with
       | "ap-northeast-1" ->
           Some "secretsmanager.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "secretsmanager.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "secretsmanager.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "secretsmanager.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "secretsmanager.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "secretsmanager.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "secretsmanager.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "secretsmanager.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "secretsmanager.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "secretsmanager.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "secretsmanager.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "secretsmanager.us-east-1.amazonaws.com"
       | "us-east-1-fips" ->
           Some "secretsmanager-fips.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "secretsmanager.us-east-2.amazonaws.com"
       | "us-east-2-fips" ->
           Some "secretsmanager-fips.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "secretsmanager.us-west-1.amazonaws.com"
       | "us-west-1-fips" ->
           Some "secretsmanager-fips.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "secretsmanager.us-west-2.amazonaws.com"
       | "us-west-2-fips" ->
           Some "secretsmanager-fips.us-west-2.amazonaws.com"
       | _ -> None)
  | "securityhub" ->
      (match region with
       | "ap-northeast-1" -> Some "securityhub.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "securityhub.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "securityhub.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "securityhub.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "securityhub.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "securityhub.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "securityhub.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "securityhub.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "securityhub.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "securityhub.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "securityhub.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "securityhub.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "securityhub.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "securityhub.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "securityhub.us-west-2.amazonaws.com"
       | _ -> None)
  | "serverlessrepo" ->
      (match region with
       | "ap-northeast-1" ->
           Some "serverlessrepo.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "serverlessrepo.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "serverlessrepo.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "serverlessrepo.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "serverlessrepo.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "serverlessrepo.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "serverlessrepo.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "serverlessrepo.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "serverlessrepo.eu-west-2.amazonaws.com"
       | "sa-east-1" -> Some "serverlessrepo.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "serverlessrepo.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "serverlessrepo.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "serverlessrepo.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "serverlessrepo.us-west-2.amazonaws.com"
       | _ -> None)
  | "servicecatalog" ->
      (match region with
       | "ap-northeast-1" ->
           Some "servicecatalog.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "servicecatalog.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "servicecatalog.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "servicecatalog.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "servicecatalog.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "servicecatalog.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "servicecatalog.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "servicecatalog.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "servicecatalog.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "servicecatalog.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "servicecatalog.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "servicecatalog.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "servicecatalog.us-east-1.amazonaws.com"
       | "us-east-1-fips" ->
           Some "servicecatalog-fips.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "servicecatalog.us-east-2.amazonaws.com"
       | "us-east-2-fips" ->
           Some "servicecatalog-fips.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "servicecatalog.us-west-1.amazonaws.com"
       | "us-west-1-fips" ->
           Some "servicecatalog-fips.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "servicecatalog.us-west-2.amazonaws.com"
       | "us-west-2-fips" ->
           Some "servicecatalog-fips.us-west-2.amazonaws.com"
       | _ -> None)
  | "servicediscovery" ->
      (match region with
       | "ap-northeast-1" ->
           Some "servicediscovery.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "servicediscovery.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "servicediscovery.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "servicediscovery.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "servicediscovery.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "servicediscovery.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "servicediscovery.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "servicediscovery.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "servicediscovery.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "servicediscovery.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "servicediscovery.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "servicediscovery.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "servicediscovery.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "servicediscovery.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "servicediscovery.us-west-2.amazonaws.com"
       | _ -> None)
  | "shield" ->
      (match region with
       | "us-east-1" -> Some "shield.us-east-1.amazonaws.com"
       | _ -> None)
  | "sms" ->
      (match region with
       | "ap-northeast-1" -> Some "sms.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "sms.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "sms.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "sms.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "sms.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "sms.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "sms.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "sms.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "sms.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "sms.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "sms.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "sms.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "sms.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "sms.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "sms.us-west-2.amazonaws.com"
       | _ -> None)
  | "snowball" ->
      (match region with
       | "ap-northeast-1" -> Some "snowball.ap-northeast-1.amazonaws.com"
       | "ap-south-1" -> Some "snowball.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "snowball.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "snowball.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "snowball.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "snowball.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "snowball.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "snowball.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "snowball.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "snowball.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "snowball.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "snowball.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "snowball.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "snowball.us-west-2.amazonaws.com"
       | _ -> None)
  | "sns" ->
      (match region with
       | "ap-northeast-1" -> Some "sns.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "sns.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "sns.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "sns.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "sns.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "sns.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "sns.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "sns.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "sns.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "sns.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "sns.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "sns.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "sns.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "sns.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "sns.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "sns.us-west-2.amazonaws.com"
       | _ -> None)
  | "sqs" ->
      (match region with
       | "ap-northeast-1" -> Some "sqs.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "sqs.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "sqs.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "sqs.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "sqs.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "sqs.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "sqs.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "sqs.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "sqs.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "sqs.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "sqs.eu-west-3.amazonaws.com"
       | "fips-us-east-1" -> Some "sqs-fips.us-east-1.amazonaws.com"
       | "fips-us-east-2" -> Some "sqs-fips.us-east-2.amazonaws.com"
       | "fips-us-west-1" -> Some "sqs-fips.us-west-1.amazonaws.com"
       | "fips-us-west-2" -> Some "sqs-fips.us-west-2.amazonaws.com"
       | "sa-east-1" -> Some "sqs.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "sqs.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "sqs.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "sqs.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "sqs.us-west-2.amazonaws.com"
       | _ -> None)
  | "ssm" ->
      (match region with
       | "ap-northeast-1" -> Some "ssm.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "ssm.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "ssm.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "ssm.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "ssm.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "ssm.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "ssm.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "ssm.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "ssm.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "ssm.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "ssm.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "ssm.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "ssm.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "ssm.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "ssm.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "ssm.us-west-2.amazonaws.com"
       | _ -> None)
  | "states" ->
      (match region with
       | "ap-northeast-1" -> Some "states.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "states.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "states.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "states.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "states.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "states.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "states.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "states.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "states.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "states.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "states.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "states.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "states.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "states.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "states.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "states.us-west-2.amazonaws.com"
       | _ -> None)
  | "storagegateway" ->
      (match region with
       | "ap-northeast-1" ->
           Some "storagegateway.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "storagegateway.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "storagegateway.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "storagegateway.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "storagegateway.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "storagegateway.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "storagegateway.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "storagegateway.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "storagegateway.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "storagegateway.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "storagegateway.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "storagegateway.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "storagegateway.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "storagegateway.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "storagegateway.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "storagegateway.us-west-2.amazonaws.com"
       | _ -> None)
  | "streams.dynamodb" ->
      (match region with
       | "ap-northeast-1" ->
           Some "streams.dynamodb.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" ->
           Some "streams.dynamodb.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "streams.dynamodb.ap-south-1.amazonaws.com"
       | "ap-southeast-1" ->
           Some "streams.dynamodb.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" ->
           Some "streams.dynamodb.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "streams.dynamodb.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "streams.dynamodb.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "streams.dynamodb.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "streams.dynamodb.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "streams.dynamodb.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "streams.dynamodb.eu-west-3.amazonaws.com"
       | "local" -> Some "localhost:8000"
       | "sa-east-1" -> Some "streams.dynamodb.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "streams.dynamodb.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "streams.dynamodb.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "streams.dynamodb.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "streams.dynamodb.us-west-2.amazonaws.com"
       | _ -> None)
  | "sts" ->
      (match region with
       | "ap-northeast-1" -> Some "sts.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "sts.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "sts.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "sts.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "sts.ap-southeast-2.amazonaws.com"
       | "aws-global" -> Some "sts.aws-global.amazonaws.com"
       | "ca-central-1" -> Some "sts.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "sts.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "sts.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "sts.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "sts.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "sts.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "sts.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "sts.us-east-1.amazonaws.com"
       | "us-east-1-fips" -> Some "sts-fips.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "sts.us-east-2.amazonaws.com"
       | "us-east-2-fips" -> Some "sts-fips.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "sts.us-west-1.amazonaws.com"
       | "us-west-1-fips" -> Some "sts-fips.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "sts.us-west-2.amazonaws.com"
       | "us-west-2-fips" -> Some "sts-fips.us-west-2.amazonaws.com"
       | _ -> None)
  | "support" ->
      (match region with
       | "us-east-1" -> Some "support.us-east-1.amazonaws.com"
       | _ -> None)
  | "swf" ->
      (match region with
       | "ap-northeast-1" -> Some "swf.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "swf.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "swf.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "swf.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "swf.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "swf.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "swf.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "swf.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "swf.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "swf.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "swf.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "swf.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "swf.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "swf.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "swf.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "swf.us-west-2.amazonaws.com"
       | _ -> None)
  | "tagging" ->
      (match region with
       | "ap-northeast-1" -> Some "tagging.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "tagging.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "tagging.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "tagging.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "tagging.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "tagging.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "tagging.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "tagging.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "tagging.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "tagging.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "tagging.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "tagging.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "tagging.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "tagging.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "tagging.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "tagging.us-west-2.amazonaws.com"
       | _ -> None)
  | "transfer" ->
      (match region with
       | "ap-northeast-1" -> Some "transfer.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "transfer.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "transfer.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "transfer.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "transfer.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "transfer.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "transfer.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "transfer.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "transfer.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "transfer.eu-west-3.amazonaws.com"
       | "us-east-1" -> Some "transfer.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "transfer.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "transfer.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "transfer.us-west-2.amazonaws.com"
       | _ -> None)
  | "translate" ->
      (match region with
       | "eu-west-1" -> Some "translate.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "translate.us-east-1.amazonaws.com"
       | "us-east-1-fips" -> Some "translate-fips.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "translate.us-east-2.amazonaws.com"
       | "us-east-2-fips" -> Some "translate-fips.us-east-2.amazonaws.com"
       | "us-west-2" -> Some "translate.us-west-2.amazonaws.com"
       | "us-west-2-fips" -> Some "translate-fips.us-west-2.amazonaws.com"
       | _ -> None)
  | "waf" ->
      (match region with
       | "aws-global" -> Some "waf.amazonaws.com"
       | _ -> None)
  | "waf-regional" ->
      (match region with
       | "ap-northeast-1" -> Some "waf-regional.ap-northeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "waf-regional.ap-southeast-2.amazonaws.com"
       | "eu-central-1" -> Some "waf-regional.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "waf-regional.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "waf-regional.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "waf-regional.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "waf-regional.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "waf-regional.us-west-2.amazonaws.com"
       | _ -> None)
  | "workdocs" ->
      (match region with
       | "ap-northeast-1" -> Some "workdocs.ap-northeast-1.amazonaws.com"
       | "ap-southeast-1" -> Some "workdocs.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "workdocs.ap-southeast-2.amazonaws.com"
       | "eu-west-1" -> Some "workdocs.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "workdocs.us-east-1.amazonaws.com"
       | "us-west-2" -> Some "workdocs.us-west-2.amazonaws.com"
       | _ -> None)
  | "workmail" ->
      (match region with
       | "eu-west-1" -> Some "workmail.eu-west-1.amazonaws.com"
       | "us-east-1" -> Some "workmail.us-east-1.amazonaws.com"
       | "us-west-2" -> Some "workmail.us-west-2.amazonaws.com"
       | _ -> None)
  | "workspaces" ->
      (match region with
       | "ap-northeast-1" -> Some "workspaces.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "workspaces.ap-northeast-2.amazonaws.com"
       | "ap-southeast-1" -> Some "workspaces.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "workspaces.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "workspaces.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "workspaces.eu-central-1.amazonaws.com"
       | "eu-west-1" -> Some "workspaces.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "workspaces.eu-west-2.amazonaws.com"
       | "sa-east-1" -> Some "workspaces.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "workspaces.us-east-1.amazonaws.com"
       | "us-west-2" -> Some "workspaces.us-west-2.amazonaws.com"
       | _ -> None)
  | "xray" ->
      (match region with
       | "ap-northeast-1" -> Some "xray.ap-northeast-1.amazonaws.com"
       | "ap-northeast-2" -> Some "xray.ap-northeast-2.amazonaws.com"
       | "ap-south-1" -> Some "xray.ap-south-1.amazonaws.com"
       | "ap-southeast-1" -> Some "xray.ap-southeast-1.amazonaws.com"
       | "ap-southeast-2" -> Some "xray.ap-southeast-2.amazonaws.com"
       | "ca-central-1" -> Some "xray.ca-central-1.amazonaws.com"
       | "eu-central-1" -> Some "xray.eu-central-1.amazonaws.com"
       | "eu-north-1" -> Some "xray.eu-north-1.amazonaws.com"
       | "eu-west-1" -> Some "xray.eu-west-1.amazonaws.com"
       | "eu-west-2" -> Some "xray.eu-west-2.amazonaws.com"
       | "eu-west-3" -> Some "xray.eu-west-3.amazonaws.com"
       | "sa-east-1" -> Some "xray.sa-east-1.amazonaws.com"
       | "us-east-1" -> Some "xray.us-east-1.amazonaws.com"
       | "us-east-2" -> Some "xray.us-east-2.amazonaws.com"
       | "us-west-1" -> Some "xray.us-west-1.amazonaws.com"
       | "us-west-2" -> Some "xray.us-west-2.amazonaws.com"
       | _ -> None)
  | _ -> None
let url_of svc_name region =
  match endpoint_of svc_name region with
  | Some var -> Some ("https://" ^ var)
  | None -> None