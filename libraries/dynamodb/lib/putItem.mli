open Types
type input = PutItemInput.t
type output = PutItemOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error