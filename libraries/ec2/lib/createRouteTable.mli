open Types
type input = CreateRouteTableRequest.t
type output = CreateRouteTableResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)