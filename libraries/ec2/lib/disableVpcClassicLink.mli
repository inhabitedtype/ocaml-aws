open Types
type input = DisableVpcClassicLinkRequest.t
type output = DisableVpcClassicLinkResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error