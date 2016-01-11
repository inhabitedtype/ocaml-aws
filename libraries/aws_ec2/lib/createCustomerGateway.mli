open Types
type input = CreateCustomerGatewayRequest.t
type output = CreateCustomerGatewayResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)