open Types
type input = AttachVpnGatewayRequest.t
type output = AttachVpnGatewayResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)