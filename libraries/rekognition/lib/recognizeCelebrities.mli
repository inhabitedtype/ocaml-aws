open Types
type input = RecognizeCelebritiesRequest.t
type output = RecognizeCelebritiesResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error