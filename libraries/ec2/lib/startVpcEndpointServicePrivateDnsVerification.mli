open Types
type input = StartVpcEndpointServicePrivateDnsVerificationRequest.t
type output = StartVpcEndpointServicePrivateDnsVerificationResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error