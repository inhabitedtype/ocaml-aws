open Types

type input = DescribeMaintenanceWindowExecutionsRequest.t

type output = DescribeMaintenanceWindowExecutionsResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
