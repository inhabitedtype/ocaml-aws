open Types
type input = DescribeTransitGatewayRouteTablesRequest.t
type output = DescribeTransitGatewayRouteTablesResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error