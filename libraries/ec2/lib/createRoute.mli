open Types
type input = CreateRouteRequest.t
type output = CreateRouteResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error