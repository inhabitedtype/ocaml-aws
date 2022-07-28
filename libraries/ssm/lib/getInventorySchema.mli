open Types

type input = GetInventorySchemaRequest.t

type output = GetInventorySchemaResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
