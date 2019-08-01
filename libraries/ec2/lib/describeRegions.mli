open Types
type input = DescribeRegionsRequest.t
type output = DescribeRegionsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error