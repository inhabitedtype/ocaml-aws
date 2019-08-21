open Types
type input = RunJobFlowInput.t
type output = RunJobFlowOutput.t
type error = Errors_internal.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)