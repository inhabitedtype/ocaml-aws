open Types
type input = DeleteStackInstancesInput.t
type output = DeleteStackInstancesOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error