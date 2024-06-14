open Types
type input = GetCelebrityRecognitionRequest.t
type output = GetCelebrityRecognitionResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error