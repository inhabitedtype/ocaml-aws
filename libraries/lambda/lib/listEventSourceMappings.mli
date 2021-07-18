open Types
type input = ListEventSourceMappingsRequest.t
type output = ListEventSourceMappingsResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error