type input = CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationRequest.t
type output = CreateLocalGatewayRouteTableVirtualInterfaceGroupAssociationResult.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
