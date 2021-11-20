open Types
type input = CreateStackSetInput.t
type output = CreateStackSetOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error