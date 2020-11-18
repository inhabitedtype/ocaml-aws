open Types
type input = UploadPartRequest.t
type output = UploadPartOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error