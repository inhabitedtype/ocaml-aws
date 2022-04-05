open Types
type input = RejectVpcEndpointConnectionsRequest.t
type output = RejectVpcEndpointConnectionsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error