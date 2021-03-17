open Types
type input = AuthorizeClientVpnIngressRequest.t
type output = AuthorizeClientVpnIngressResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error