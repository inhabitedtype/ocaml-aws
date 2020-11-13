open Types
type input = GetGeoLocationRequest.t
type output = GetGeoLocationResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error