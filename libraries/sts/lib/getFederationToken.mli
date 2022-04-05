open Types
type input = GetFederationTokenRequest.t
type output = GetFederationTokenResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error