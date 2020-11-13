open Types
type input = CreateDBInstanceMessage.t
type output = CreateDBInstanceResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error