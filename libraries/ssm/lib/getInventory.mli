open Types

type input = GetInventoryRequest.t

type output = GetInventoryResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
