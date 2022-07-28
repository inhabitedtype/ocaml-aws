open Types

type input = PutParameterRequest.t

type output = PutParameterResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
