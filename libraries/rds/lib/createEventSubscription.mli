open Types
type input = CreateEventSubscriptionMessage.t
type output = CreateEventSubscriptionResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)