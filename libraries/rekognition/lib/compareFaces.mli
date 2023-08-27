open Types
type input = CompareFacesRequest.t
type output = CompareFacesResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error