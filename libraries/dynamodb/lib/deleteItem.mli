open Types
type input = DeleteItemInput.t
type output = DeleteItemOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error