open Types
type input = ListInstancesInput.t
type output = ListInstancesOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error