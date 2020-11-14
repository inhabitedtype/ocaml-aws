open Types

type input = GetBucketNotificationConfigurationRequest.t

type output = NotificationConfiguration.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
