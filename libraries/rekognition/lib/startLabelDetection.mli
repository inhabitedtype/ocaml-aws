open Types
type input = StartLabelDetectionRequest.t
type output = StartLabelDetectionResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error