open Types

type input = RegisterTaskWithMaintenanceWindowRequest.t

type output = RegisterTaskWithMaintenanceWindowResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
