open Types
type input = ModifyVpnTunnelCertificateRequest.t
type output = ModifyVpnTunnelCertificateResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error