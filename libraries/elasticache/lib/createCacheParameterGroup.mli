open Types
type input = CreateCacheParameterGroupMessage.t
type output = CreateCacheParameterGroupResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)