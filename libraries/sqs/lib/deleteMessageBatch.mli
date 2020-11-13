open Types
type input = DeleteMessageBatchRequest.t
type output = DeleteMessageBatchResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error