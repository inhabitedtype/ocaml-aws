open Types
type input = UpdateItemInput.t
type output = UpdateItemOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error