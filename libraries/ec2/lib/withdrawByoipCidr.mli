open Types
type input = WithdrawByoipCidrRequest.t
type output = WithdrawByoipCidrResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error