open Types

type input = DescribeMaintenanceWindowsForTargetRequest.t

type output = DescribeMaintenanceWindowsForTargetResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
