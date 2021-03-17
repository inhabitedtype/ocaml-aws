open Types
type input = DescribeVpcEndpointsRequest.t
type output = DescribeVpcEndpointsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error