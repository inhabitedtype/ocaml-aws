open Types
type input = DescribeOptionGroupsMessage.t
type output = OptionGroups.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error