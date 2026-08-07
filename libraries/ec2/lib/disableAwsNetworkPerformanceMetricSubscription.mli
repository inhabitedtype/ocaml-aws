type input = DisableAwsNetworkPerformanceMetricSubscriptionRequest.t
type output = DisableAwsNetworkPerformanceMetricSubscriptionResult.t
type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
