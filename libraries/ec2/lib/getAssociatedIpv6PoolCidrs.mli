open Types
type input = GetAssociatedIpv6PoolCidrsRequest.t
type output = GetAssociatedIpv6PoolCidrsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error