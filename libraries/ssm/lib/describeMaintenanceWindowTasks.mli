open Types

type input = DescribeMaintenanceWindowTasksRequest.t

type output = DescribeMaintenanceWindowTasksResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
