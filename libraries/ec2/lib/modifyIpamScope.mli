type input = ModifyIpamScopeRequest.t
type output = ModifyIpamScopeResult.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
