open Types
type input = CreateStackInput.t
type output = CreateStackOutput.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)