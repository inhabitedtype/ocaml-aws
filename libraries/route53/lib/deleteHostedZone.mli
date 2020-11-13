open Types
type input = DeleteHostedZoneRequest.t
type output = DeleteHostedZoneResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error