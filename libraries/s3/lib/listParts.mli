open Types
type input = ListPartsRequest.t
type output = ListPartsOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error