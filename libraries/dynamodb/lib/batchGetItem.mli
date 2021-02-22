open Types
type input = BatchGetItemInput.t
type output = BatchGetItemOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error