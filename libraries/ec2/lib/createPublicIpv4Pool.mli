type input = CreatePublicIpv4PoolRequest.t
type output = CreatePublicIpv4PoolResult.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
