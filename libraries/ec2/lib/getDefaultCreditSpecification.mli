open Types
type input = GetDefaultCreditSpecificationRequest.t
type output = GetDefaultCreditSpecificationResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error