open Types
type input = DeleteEgressOnlyInternetGatewayRequest.t
type output = DeleteEgressOnlyInternetGatewayResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error