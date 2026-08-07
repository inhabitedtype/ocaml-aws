type input = DescribeServerlessV2PlatformVersionsMessage.t
type output = ServerlessV2PlatformVersionsMessage.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
