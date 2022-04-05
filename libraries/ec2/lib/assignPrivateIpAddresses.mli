open Types
type input = AssignPrivateIpAddressesRequest.t
type output = AssignPrivateIpAddressesResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error