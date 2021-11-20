open Types
type input = ListDashboardsInput.t
type output = ListDashboardsOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error