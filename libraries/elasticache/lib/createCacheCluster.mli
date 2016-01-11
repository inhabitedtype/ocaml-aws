open Types
type input = CreateCacheClusterMessage.t
type output = CreateCacheClusterResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)