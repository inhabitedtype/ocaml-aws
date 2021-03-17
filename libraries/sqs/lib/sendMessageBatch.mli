open Types
type input = SendMessageBatchRequest.t
type output = SendMessageBatchResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error