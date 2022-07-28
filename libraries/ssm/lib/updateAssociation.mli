open Types

type input = UpdateAssociationRequest.t

type output = UpdateAssociationResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
