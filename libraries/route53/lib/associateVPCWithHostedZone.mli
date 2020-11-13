open Types
type input = AssociateVPCWithHostedZoneRequest.t
type output = AssociateVPCWithHostedZoneResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error