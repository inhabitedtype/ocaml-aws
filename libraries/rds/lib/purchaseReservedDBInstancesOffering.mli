open Types
type input = PurchaseReservedDBInstancesOfferingMessage.t
type output = PurchaseReservedDBInstancesOfferingResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)