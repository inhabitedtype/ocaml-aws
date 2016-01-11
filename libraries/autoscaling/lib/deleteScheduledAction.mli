open Types
type input = DeleteScheduledActionType.t
type output = unit
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)