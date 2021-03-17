open Types
type input = DeprovisionByoipCidrRequest.t
type output = DeprovisionByoipCidrResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error