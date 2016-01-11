open Types
type input = CreateTrailRequest.t
type output = CreateTrailResponse.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)