type input = DisassociateTransitGatewayPolicyTableRequest.t
type output = DisassociateTransitGatewayPolicyTableResult.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
