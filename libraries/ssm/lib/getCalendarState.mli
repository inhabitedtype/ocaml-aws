open Types

type input = GetCalendarStateRequest.t

type output = GetCalendarStateResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
