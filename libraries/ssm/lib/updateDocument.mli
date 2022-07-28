open Types

type input = UpdateDocumentRequest.t

type output = UpdateDocumentResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
