open Types
type input = DescribeUserGroupsMessage.t
type output = DescribeUserGroupsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error