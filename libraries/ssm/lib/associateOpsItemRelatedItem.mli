open Types

type input = AssociateOpsItemRelatedItemRequest.t
type output = AssociateOpsItemRelatedItemResponse.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
