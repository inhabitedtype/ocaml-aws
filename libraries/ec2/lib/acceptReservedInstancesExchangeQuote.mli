open Types
type input = AcceptReservedInstancesExchangeQuoteRequest.t
type output = AcceptReservedInstancesExchangeQuoteResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error