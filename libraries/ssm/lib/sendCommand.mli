open Types

type input = SendCommandRequest.t

type output = SendCommandResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
