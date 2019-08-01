open Types
type input = RegisterImageRequest.t
type output = RegisterImageResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error