open Types
type input = ListStackInstancesInput.t
type output = ListStackInstancesOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error