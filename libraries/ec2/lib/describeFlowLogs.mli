open Types
type input = DescribeFlowLogsRequest.t
type output = DescribeFlowLogsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error