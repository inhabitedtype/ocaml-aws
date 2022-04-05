open Types
type input = ListTypesInput.t
type output = ListTypesOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error