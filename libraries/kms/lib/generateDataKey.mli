open Types

type input = GenerateDataKeyRequest.t

type output = GenerateDataKeyResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
