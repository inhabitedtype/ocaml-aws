open Types
type input = DescribeDBParametersMessage.t
type output = DBParameterGroupDetails.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)