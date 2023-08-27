open Types
type input = GetCelebrityInfoRequest.t
type output = GetCelebrityInfoResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error