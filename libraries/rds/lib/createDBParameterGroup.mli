open Types
type input = CreateDBParameterGroupMessage.t
type output = CreateDBParameterGroupResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)