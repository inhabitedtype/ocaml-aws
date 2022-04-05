open Types
type input = DescribeExportImageTasksRequest.t
type output = DescribeExportImageTasksResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error