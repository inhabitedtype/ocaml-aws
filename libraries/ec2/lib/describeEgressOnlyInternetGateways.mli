open Types
type input = DescribeEgressOnlyInternetGatewaysRequest.t
type output = DescribeEgressOnlyInternetGatewaysResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error