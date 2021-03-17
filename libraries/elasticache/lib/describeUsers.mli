open Types
type input = DescribeUsersMessage.t
type output = DescribeUsersResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error