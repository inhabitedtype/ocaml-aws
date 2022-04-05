open Types
type input = ModifyGlobalClusterMessage.t
type output = ModifyGlobalClusterResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error