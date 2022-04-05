open Types
type input = EnableVpcClassicLinkDnsSupportRequest.t
type output = EnableVpcClassicLinkDnsSupportResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error