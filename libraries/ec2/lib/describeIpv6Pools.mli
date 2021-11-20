open Types
type input = DescribeIpv6PoolsRequest.t
type output = DescribeIpv6PoolsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error