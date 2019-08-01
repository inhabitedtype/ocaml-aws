open Types
type input = DescribeEventCategoriesMessage.t
type output = EventCategoriesMessage.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error