open Types
type input = ExitStandbyQuery.t
type output = ExitStandbyAnswer.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error