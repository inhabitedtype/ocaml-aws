open Types
type input = GetTransitGatewayRouteTableAssociationsRequest.t
type output = GetTransitGatewayRouteTableAssociationsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error