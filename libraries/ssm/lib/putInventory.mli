open Types

type input = PutInventoryRequest.t
type output = PutInventoryResult.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
