open Types

type input = UpdateKeyDescriptionRequest.t

type output = unit

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
