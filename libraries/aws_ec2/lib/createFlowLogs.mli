open Types
type input = CreateFlowLogsRequest.t
type output = CreateFlowLogsResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)