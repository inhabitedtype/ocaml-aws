open Types
type input = DeleteCacheClusterMessage.t
type output = DeleteCacheClusterResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error