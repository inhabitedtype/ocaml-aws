open Types
type input = DisableEbsEncryptionByDefaultRequest.t
type output = DisableEbsEncryptionByDefaultResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error