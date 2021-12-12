open Types

type input = GenerateDataKeyPairRequest.t

type output = GenerateDataKeyPairResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
