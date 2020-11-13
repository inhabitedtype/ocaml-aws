open Types
type input = FailoverDBClusterMessage.t
type output = FailoverDBClusterResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error