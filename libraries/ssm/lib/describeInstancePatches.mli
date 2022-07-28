open Types

type input = DescribeInstancePatchesRequest.t

type output = DescribeInstancePatchesResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
