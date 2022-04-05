open Types
type input = GetTrailRequest.t
type output = GetTrailResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error