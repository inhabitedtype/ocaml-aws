open Types

type input = ReplicateKeyRequest.t

type output = ReplicateKeyResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
