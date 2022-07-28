open Types

type input = GetConnectionStatusRequest.t

type output = GetConnectionStatusResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
