open Types
type input = DescribeScalingActivitiesType.t
type output = ActivitiesType.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)