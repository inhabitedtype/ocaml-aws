open Types
type input = ListStackResourcesInput.t
type output = ListStackResourcesOutput.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)