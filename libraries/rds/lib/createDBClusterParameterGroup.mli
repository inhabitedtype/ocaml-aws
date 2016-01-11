open Types
type input = CreateDBClusterParameterGroupMessage.t
type output = CreateDBClusterParameterGroupResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)