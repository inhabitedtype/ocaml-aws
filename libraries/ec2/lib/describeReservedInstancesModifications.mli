open Types
type input = DescribeReservedInstancesModificationsRequest.t
type output = DescribeReservedInstancesModificationsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error