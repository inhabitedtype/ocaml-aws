open Types

type input = CancelKeyDeletionRequest.t

type output = CancelKeyDeletionResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
