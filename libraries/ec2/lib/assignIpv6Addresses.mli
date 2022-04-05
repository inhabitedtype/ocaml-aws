open Types
type input = AssignIpv6AddressesRequest.t
type output = AssignIpv6AddressesResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error