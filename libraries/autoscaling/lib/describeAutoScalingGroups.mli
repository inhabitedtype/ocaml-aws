open Types
type input = AutoScalingGroupNamesType.t
type output = AutoScalingGroupsType.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)