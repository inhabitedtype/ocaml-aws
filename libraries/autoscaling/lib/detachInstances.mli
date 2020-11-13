open Types
type input = DetachInstancesQuery.t
type output = DetachInstancesAnswer.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error