open Types
type input = GetMetricStatisticsInput.t
type output = GetMetricStatisticsOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error