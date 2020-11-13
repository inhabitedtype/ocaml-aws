open Types
type input = AddSourceIdentifierToSubscriptionMessage.t
type output = AddSourceIdentifierToSubscriptionResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error