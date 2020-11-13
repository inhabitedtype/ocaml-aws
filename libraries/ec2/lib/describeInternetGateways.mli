open Types
type input = DescribeInternetGatewaysRequest.t
type output = DescribeInternetGatewaysResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error