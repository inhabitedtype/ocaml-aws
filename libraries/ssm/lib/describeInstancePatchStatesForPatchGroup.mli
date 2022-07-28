open Types

type input = DescribeInstancePatchStatesForPatchGroupRequest.t

type output = DescribeInstancePatchStatesForPatchGroupResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
