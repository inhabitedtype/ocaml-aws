open Types
type input = GetFaceDetectionRequest.t
type output = GetFaceDetectionResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error