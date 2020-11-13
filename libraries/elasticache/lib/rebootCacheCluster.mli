open Types
type input = RebootCacheClusterMessage.t
type output = RebootCacheClusterResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error