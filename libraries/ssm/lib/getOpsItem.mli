open Types

type input = GetOpsItemRequest.t

type output = GetOpsItemResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
