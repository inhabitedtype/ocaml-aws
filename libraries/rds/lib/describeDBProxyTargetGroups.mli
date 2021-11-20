open Types
type input = DescribeDBProxyTargetGroupsRequest.t
type output = DescribeDBProxyTargetGroupsResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error