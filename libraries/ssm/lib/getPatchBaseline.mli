open Types

type input = GetPatchBaselineRequest.t

type output = GetPatchBaselineResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
