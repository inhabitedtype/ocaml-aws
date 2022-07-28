open Types

type input = DeleteMaintenanceWindowRequest.t

type output = DeleteMaintenanceWindowResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
