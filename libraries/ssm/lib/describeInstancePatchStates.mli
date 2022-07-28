open Types

type input = DescribeInstancePatchStatesRequest.t

type output = DescribeInstancePatchStatesResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
