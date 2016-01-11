open Types
type input = ModifyCacheParameterGroupMessage.t
type output = CacheParameterGroupNameMessage.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)