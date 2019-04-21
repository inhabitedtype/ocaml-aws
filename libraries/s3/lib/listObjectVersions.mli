open Types_internal
type input = ListObjectVersionsRequest.t
type output = ListObjectVersionsOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error