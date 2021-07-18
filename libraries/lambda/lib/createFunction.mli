open Types
type input = CreateFunctionRequest.t
type output = FunctionConfiguration.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error