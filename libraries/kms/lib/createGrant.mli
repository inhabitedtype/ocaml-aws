open Types

type input = CreateGrantRequest.t

type output = CreateGrantResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
