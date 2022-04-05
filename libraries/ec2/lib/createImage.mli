open Types
type input = CreateImageRequest.t
type output = CreateImageResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error