type input = DisableImageRequest.t
type output = DisableImageResult.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
