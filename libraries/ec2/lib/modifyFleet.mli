open Types
type input = ModifyFleetRequest.t
type output = ModifyFleetResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error