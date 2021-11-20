open Types
type input = CreatePlacementGroupRequest.t
type output = CreatePlacementGroupResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error