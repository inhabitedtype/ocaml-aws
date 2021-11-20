open Types
type input = DeleteDBInstanceAutomatedBackupMessage.t
type output = DeleteDBInstanceAutomatedBackupResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error