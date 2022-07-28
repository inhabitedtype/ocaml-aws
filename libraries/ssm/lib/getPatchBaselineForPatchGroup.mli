open Types

type input = GetPatchBaselineForPatchGroupRequest.t

type output = GetPatchBaselineForPatchGroupResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
