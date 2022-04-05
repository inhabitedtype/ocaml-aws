open Types
type input = DisassociateTransitGatewayMulticastDomainRequest.t
type output = DisassociateTransitGatewayMulticastDomainResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error