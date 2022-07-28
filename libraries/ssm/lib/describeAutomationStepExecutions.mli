open Types

type input = DescribeAutomationStepExecutionsRequest.t

type output = DescribeAutomationStepExecutionsResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
