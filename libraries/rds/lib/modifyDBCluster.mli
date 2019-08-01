open Types
type input = ModifyDBClusterMessage.t
type output = ModifyDBClusterResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error