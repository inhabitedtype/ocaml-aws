open Types
type input = ListFunctionsRequest.t
type output = ListFunctionsResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error