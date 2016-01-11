open Types
type input = SelectRequest.t
type output = SelectResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)