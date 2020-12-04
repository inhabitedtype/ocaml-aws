open Types
type input = StopActivityStreamRequest.t
type output = StopActivityStreamResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error