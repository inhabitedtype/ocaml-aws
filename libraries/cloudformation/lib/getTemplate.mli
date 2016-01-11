open Types
type input = GetTemplateInput.t
type output = GetTemplateOutput.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)