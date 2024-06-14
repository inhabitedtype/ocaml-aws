open Types
type input = GetPersonTrackingRequest.t
type output = GetPersonTrackingResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error