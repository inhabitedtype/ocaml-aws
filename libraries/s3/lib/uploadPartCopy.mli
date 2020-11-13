open Types
type input = UploadPartCopyRequest.t
type output = UploadPartCopyOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error