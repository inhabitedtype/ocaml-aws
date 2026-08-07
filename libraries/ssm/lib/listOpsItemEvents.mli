open Types

type input = ListOpsItemEventsRequest.t
type output = ListOpsItemEventsResponse.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
