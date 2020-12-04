open Types
type input = RestoreDBInstanceFromS3Message.t
type output = RestoreDBInstanceFromS3Result.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error