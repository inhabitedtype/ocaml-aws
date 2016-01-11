open Types
type input = ImportImageRequest.t
type output = ImportImageResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)