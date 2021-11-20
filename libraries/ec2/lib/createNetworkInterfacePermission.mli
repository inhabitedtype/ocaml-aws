open Types
type input = CreateNetworkInterfacePermissionRequest.t
type output = CreateNetworkInterfacePermissionResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error