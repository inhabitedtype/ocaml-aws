open Types
type input = DescribeSubnetsRequest.t
type output = DescribeSubnetsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error