open Types
type input = CreateCacheSubnetGroupMessage.t
type output = CreateCacheSubnetGroupResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error