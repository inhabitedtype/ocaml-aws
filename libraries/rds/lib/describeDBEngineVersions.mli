open Types
type input = DescribeDBEngineVersionsMessage.t
type output = DBEngineVersionMessage.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)