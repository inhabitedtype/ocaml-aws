open Types
type input = AcceptTransitGatewayPeeringAttachmentRequest.t
type output = AcceptTransitGatewayPeeringAttachmentResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error