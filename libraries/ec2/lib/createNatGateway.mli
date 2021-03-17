open Types
type input = CreateNatGatewayRequest.t
type output = CreateNatGatewayResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error