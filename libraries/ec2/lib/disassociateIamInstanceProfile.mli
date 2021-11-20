open Types
type input = DisassociateIamInstanceProfileRequest.t
type output = DisassociateIamInstanceProfileResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error