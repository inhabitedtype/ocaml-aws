open Types
type input = DeregisterEndPointsInput.t
type output = DeregisterEndPointsOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error