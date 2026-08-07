type input = GetInstanceUefiDataRequest.t
type output = GetInstanceUefiDataResult.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
