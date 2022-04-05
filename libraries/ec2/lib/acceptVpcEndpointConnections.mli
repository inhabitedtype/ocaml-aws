open Types
type input = AcceptVpcEndpointConnectionsRequest.t
type output = AcceptVpcEndpointConnectionsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error