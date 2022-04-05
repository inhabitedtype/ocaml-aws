open Types
type input = DeleteCarrierGatewayRequest.t
type output = DeleteCarrierGatewayResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error