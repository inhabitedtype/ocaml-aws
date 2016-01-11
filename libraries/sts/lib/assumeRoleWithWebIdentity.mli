open Types
type input = AssumeRoleWithWebIdentityRequest.t
type output = AssumeRoleWithWebIdentityResponse.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)