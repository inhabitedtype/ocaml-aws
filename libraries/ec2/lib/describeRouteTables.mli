open Types
type input = DescribeRouteTablesRequest.t
type output = DescribeRouteTablesResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error