open Types

type input = DescribeMaintenanceWindowScheduleRequest.t

type output = DescribeMaintenanceWindowScheduleResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
