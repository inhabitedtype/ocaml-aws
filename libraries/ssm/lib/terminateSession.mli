open Types

type input = TerminateSessionRequest.t

type output = TerminateSessionResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
