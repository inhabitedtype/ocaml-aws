open Types
type input = CopyDBClusterParameterGroupMessage.t
type output = CopyDBClusterParameterGroupResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error