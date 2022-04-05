open Types
type input = UpdateTerminationProtectionInput.t
type output = UpdateTerminationProtectionOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error