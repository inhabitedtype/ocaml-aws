open Types
type input = SendMessageRequest.t
type output = SendMessageResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error