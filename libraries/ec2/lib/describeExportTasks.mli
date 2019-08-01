open Types
type input = DescribeExportTasksRequest.t
type output = DescribeExportTasksResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error