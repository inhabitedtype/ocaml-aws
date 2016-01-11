open Types
type input = DescribeDBParameterGroupsMessage.t
type output = DBParameterGroupsMessage.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)