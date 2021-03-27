open Types
type input = GetEventSourceMappingRequest.t
type output = EventSourceMappingConfiguration.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error