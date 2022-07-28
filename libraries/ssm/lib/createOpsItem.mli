open Types

type input = CreateOpsItemRequest.t

type output = CreateOpsItemResponse.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
