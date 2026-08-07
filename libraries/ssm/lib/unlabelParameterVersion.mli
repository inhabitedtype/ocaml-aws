open Types

type input = UnlabelParameterVersionRequest.t
type output = UnlabelParameterVersionResult.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
