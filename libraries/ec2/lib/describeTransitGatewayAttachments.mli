open Types
type input = DescribeTransitGatewayAttachmentsRequest.t
type output = DescribeTransitGatewayAttachmentsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error