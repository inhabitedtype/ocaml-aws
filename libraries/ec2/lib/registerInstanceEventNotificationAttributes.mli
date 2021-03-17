open Types
type input = RegisterInstanceEventNotificationAttributesRequest.t
type output = RegisterInstanceEventNotificationAttributesResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error