open Types
type input = CreateStackInstancesInput.t
type output = CreateStackInstancesOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error