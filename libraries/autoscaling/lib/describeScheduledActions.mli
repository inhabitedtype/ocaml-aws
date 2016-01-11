open Types
type input = DescribeScheduledActionsType.t
type output = ScheduledActionsType.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)