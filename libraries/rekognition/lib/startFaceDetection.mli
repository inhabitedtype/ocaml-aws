open Types
type input = StartFaceDetectionRequest.t
type output = StartFaceDetectionResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error