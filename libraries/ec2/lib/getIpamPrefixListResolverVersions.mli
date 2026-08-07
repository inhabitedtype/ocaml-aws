type input = GetIpamPrefixListResolverVersionsRequest.t
type output = GetIpamPrefixListResolverVersionsResult.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
