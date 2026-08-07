type input = DeprovisionPublicIpv4PoolCidrRequest.t
type output = DeprovisionPublicIpv4PoolCidrResult.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
