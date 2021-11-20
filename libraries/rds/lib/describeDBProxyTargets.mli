open Types
type input = DescribeDBProxyTargetsRequest.t
type output = DescribeDBProxyTargetsResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error