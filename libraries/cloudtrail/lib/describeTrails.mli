open Types
type input = DescribeTrailsRequest.t
type output = DescribeTrailsResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error