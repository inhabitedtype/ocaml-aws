open Types
type input = CreateNetworkInterfaceRequest.t
type output = CreateNetworkInterfaceResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error