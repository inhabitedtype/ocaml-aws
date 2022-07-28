open Types

type input = DescribeAssociationExecutionsRequest.t

type output = DescribeAssociationExecutionsResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
