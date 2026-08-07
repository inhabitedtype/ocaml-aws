type input = CreateCoipPoolRequest.t
type output = CreateCoipPoolResult.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
