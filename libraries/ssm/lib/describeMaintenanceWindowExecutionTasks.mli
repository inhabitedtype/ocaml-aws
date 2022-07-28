open Types

type input = DescribeMaintenanceWindowExecutionTasksRequest.t

type output = DescribeMaintenanceWindowExecutionTasksResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
