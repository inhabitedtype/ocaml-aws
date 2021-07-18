open Types
type input = AddPermissionRequest.t
type output = AddPermissionResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error