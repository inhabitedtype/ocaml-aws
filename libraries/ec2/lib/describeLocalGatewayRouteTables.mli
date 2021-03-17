open Types
type input = DescribeLocalGatewayRouteTablesRequest.t
type output = DescribeLocalGatewayRouteTablesResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error