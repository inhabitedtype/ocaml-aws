open Types

type input = DecryptRequest.t

type output = DecryptResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
