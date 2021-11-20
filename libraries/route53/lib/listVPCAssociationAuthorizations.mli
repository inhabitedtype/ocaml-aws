open Types
type input = ListVPCAssociationAuthorizationsRequest.t
type output = ListVPCAssociationAuthorizationsResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error