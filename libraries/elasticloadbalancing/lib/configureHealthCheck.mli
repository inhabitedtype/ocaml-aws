open Types
type input = ConfigureHealthCheckInput.t
type output = ConfigureHealthCheckOutput.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)