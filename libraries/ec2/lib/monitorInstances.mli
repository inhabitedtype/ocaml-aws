open Types
type input = MonitorInstancesRequest.t
type output = MonitorInstancesResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error