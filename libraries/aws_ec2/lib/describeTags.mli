open Types
type input = DescribeTagsRequest.t
type output = DescribeTagsResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)