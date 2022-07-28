open Types

type input = ResumeSessionRequest.t

type output = ResumeSessionResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
