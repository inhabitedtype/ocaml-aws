open Types_internal
type input = ListHostedZonesByNameRequest.t
type output = ListHostedZonesByNameResponse.t
type error = Errors_internal.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)