open Types
type input = DescribeCacheSubnetGroupsMessage.t
type output = CacheSubnetGroupMessage.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error