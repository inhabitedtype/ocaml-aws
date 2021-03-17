open Types
type input = RejectTransitGatewayVpcAttachmentRequest.t
type output = RejectTransitGatewayVpcAttachmentResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error