open Types
type input = EnableVpcClassicLinkRequest.t
type output = EnableVpcClassicLinkResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error