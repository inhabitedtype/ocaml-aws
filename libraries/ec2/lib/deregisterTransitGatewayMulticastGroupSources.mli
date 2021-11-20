open Types
type input = DeregisterTransitGatewayMulticastGroupSourcesRequest.t
type output = DeregisterTransitGatewayMulticastGroupSourcesResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error