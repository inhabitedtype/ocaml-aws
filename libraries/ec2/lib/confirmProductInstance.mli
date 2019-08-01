open Types
type input = ConfirmProductInstanceRequest.t
type output = ConfirmProductInstanceResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error