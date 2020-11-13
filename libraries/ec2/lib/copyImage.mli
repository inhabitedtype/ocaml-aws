open Types
type input = CopyImageRequest.t
type output = CopyImageResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error