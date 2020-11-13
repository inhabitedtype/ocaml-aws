open Types
type input = DescribeReservedCacheNodesMessage.t
type output = ReservedCacheNodeMessage.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error