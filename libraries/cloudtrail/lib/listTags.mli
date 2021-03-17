open Types
type input = ListTagsRequest.t
type output = ListTagsResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error