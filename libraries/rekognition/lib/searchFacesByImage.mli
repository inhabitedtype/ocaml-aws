open Types
type input = SearchFacesByImageRequest.t
type output = SearchFacesByImageResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error