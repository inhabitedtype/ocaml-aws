open Types
type input = DeleteFpgaImageRequest.t
type output = DeleteFpgaImageResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error