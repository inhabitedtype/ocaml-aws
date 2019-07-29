open Types
type input = DeleteFacesRequest.t
type output = DeleteFacesResponse.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error