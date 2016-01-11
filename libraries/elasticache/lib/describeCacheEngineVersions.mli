open Types
type input = DescribeCacheEngineVersionsMessage.t
type output = CacheEngineVersionMessage.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)