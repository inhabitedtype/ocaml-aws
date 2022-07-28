open Types

type input = UpdatePatchBaselineRequest.t

type output = UpdatePatchBaselineResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
