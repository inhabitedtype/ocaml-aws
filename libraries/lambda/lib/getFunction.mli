open Types
type input = GetFunctionRequest.t
type output = GetFunctionResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error