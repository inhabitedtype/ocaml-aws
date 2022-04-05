open Types
type input = ModifyVpnConnectionOptionsRequest.t
type output = ModifyVpnConnectionOptionsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error