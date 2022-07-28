open Types

type input = DescribeEffectivePatchesForPatchBaselineRequest.t

type output = DescribeEffectivePatchesForPatchBaselineResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
