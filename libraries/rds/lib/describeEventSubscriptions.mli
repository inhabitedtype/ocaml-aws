open Types_internal
type input = DescribeEventSubscriptionsMessage.t
type output = EventSubscriptionsMessage.t
type error = Errors_internal.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)