open Types
type input = RunScheduledInstancesRequest.t
type output = RunScheduledInstancesResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error