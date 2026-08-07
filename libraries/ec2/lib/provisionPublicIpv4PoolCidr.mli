type input = ProvisionPublicIpv4PoolCidrRequest.t
type output = ProvisionPublicIpv4PoolCidrResult.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
