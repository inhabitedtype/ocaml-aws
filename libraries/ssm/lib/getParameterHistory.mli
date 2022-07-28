open Types

type input = GetParameterHistoryRequest.t

type output = GetParameterHistoryResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
