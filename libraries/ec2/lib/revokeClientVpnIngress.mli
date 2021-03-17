open Types
type input = RevokeClientVpnIngressRequest.t
type output = RevokeClientVpnIngressResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error