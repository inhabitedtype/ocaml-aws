open Types
type input = ModifyDefaultCreditSpecificationRequest.t
type output = ModifyDefaultCreditSpecificationResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error