open Types
type input = DescribeCacheParametersMessage.t
type output = CacheParameterGroupDetails.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)