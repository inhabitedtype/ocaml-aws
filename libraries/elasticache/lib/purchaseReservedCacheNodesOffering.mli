open Types
type input = PurchaseReservedCacheNodesOfferingMessage.t
type output = PurchaseReservedCacheNodesOfferingResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)