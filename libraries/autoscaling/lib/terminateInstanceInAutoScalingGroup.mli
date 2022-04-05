open Types
type input = TerminateInstanceInAutoScalingGroupType.t
type output = ActivityType.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error