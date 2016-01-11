open Types
type input = CreateVpnConnectionRequest.t
type output = CreateVpnConnectionResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)