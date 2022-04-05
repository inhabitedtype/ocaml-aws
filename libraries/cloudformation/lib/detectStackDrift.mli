open Types
type input = DetectStackDriftInput.t
type output = DetectStackDriftOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error