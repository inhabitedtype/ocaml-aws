open Types
type input = CreateTransitGatewayVpcAttachmentRequest.t
type output = CreateTransitGatewayVpcAttachmentResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error