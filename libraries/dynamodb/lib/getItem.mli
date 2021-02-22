open Types
type input = GetItemInput.t
type output = GetItemOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error