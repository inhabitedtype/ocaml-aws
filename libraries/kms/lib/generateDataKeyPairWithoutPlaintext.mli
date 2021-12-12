open Types

type input = GenerateDataKeyPairWithoutPlaintextRequest.t

type output = GenerateDataKeyPairWithoutPlaintextResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
