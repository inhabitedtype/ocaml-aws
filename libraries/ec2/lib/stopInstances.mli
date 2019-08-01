open Types
type input = StopInstancesRequest.t
type output = StopInstancesResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error