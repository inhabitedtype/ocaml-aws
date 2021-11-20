open Types
type input = CreateVPCAssociationAuthorizationRequest.t
type output = CreateVPCAssociationAuthorizationResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error