open Types
type input = ListHealthChecksRequest.t
type output = ListHealthChecksResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error