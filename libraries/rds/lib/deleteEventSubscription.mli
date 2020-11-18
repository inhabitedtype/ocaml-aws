open Types
type input = DeleteEventSubscriptionMessage.t
type output = DeleteEventSubscriptionResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error