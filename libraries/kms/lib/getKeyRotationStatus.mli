open Types

type input = GetKeyRotationStatusRequest.t

type output = GetKeyRotationStatusResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
