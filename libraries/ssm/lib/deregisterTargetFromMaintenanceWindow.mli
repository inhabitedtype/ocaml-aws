open Types

type input = DeregisterTargetFromMaintenanceWindowRequest.t

type output = DeregisterTargetFromMaintenanceWindowResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
