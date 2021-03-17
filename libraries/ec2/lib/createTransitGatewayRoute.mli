open Types
type input = CreateTransitGatewayRouteRequest.t
type output = CreateTransitGatewayRouteResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error