open Types
type input = DescribeReservedDBInstancesMessage.t
type output = ReservedDBInstanceMessage.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)