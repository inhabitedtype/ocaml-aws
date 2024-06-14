open Types
type input = IndexFacesRequest.t
type output = IndexFacesResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error