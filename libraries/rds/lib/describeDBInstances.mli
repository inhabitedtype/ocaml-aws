open Types
type input = DescribeDBInstancesMessage.t
type output = DBInstanceMessage.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error