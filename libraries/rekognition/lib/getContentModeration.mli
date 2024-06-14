open Types
type input = GetContentModerationRequest.t
type output = GetContentModerationResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error