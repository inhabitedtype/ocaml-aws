open Types
type input = DescribePublicIpv4PoolsRequest.t
type output = DescribePublicIpv4PoolsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error