open Types
type input = DescribeHostsRequest.t
type output = DescribeHostsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error