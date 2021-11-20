open Types
type input = GetReservedInstancesExchangeQuoteRequest.t
type output = GetReservedInstancesExchangeQuoteResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error