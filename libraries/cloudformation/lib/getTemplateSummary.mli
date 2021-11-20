open Types
type input = GetTemplateSummaryInput.t
type output = GetTemplateSummaryOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error