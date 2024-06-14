open Types
type input = CreateStreamProcessorRequest.t
type output = CreateStreamProcessorResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error