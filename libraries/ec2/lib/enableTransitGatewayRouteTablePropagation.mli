open Types
type input = EnableTransitGatewayRouteTablePropagationRequest.t
type output = EnableTransitGatewayRouteTablePropagationResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error