open Types
type input = UpdateTrailRequest.t
type output = UpdateTrailResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error