open Types

type input = GenerateRandomRequest.t

type output = GenerateRandomResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
