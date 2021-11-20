open Types
type input = RegisterTransitGatewayMulticastGroupSourcesRequest.t
type output = RegisterTransitGatewayMulticastGroupSourcesResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error