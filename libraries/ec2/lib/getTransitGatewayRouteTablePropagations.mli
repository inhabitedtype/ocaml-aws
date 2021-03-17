open Types
type input = GetTransitGatewayRouteTablePropagationsRequest.t
type output = GetTransitGatewayRouteTablePropagationsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error