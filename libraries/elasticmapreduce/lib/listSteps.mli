open Types
type input = ListStepsInput.t
type output = ListStepsOutput.t
type error = Errors_internal.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)