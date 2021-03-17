open Types
type input = DisableInsightRulesInput.t
type output = DisableInsightRulesOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error