open Types_internal
type input = DeleteDBInstanceMessage.t
type output = DeleteDBInstanceResult.t
type error = Errors_internal.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)