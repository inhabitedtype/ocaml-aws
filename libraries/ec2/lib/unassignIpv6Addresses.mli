open Types
type input = UnassignIpv6AddressesRequest.t
type output = UnassignIpv6AddressesResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error