open Types
type input = DeleteTransitGatewayPeeringAttachmentRequest.t
type output = DeleteTransitGatewayPeeringAttachmentResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error