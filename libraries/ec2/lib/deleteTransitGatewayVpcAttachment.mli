open Types
type input = DeleteTransitGatewayVpcAttachmentRequest.t
type output = DeleteTransitGatewayVpcAttachmentResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error