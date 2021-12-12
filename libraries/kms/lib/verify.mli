open Types

type input = VerifyRequest.t

type output = VerifyResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
