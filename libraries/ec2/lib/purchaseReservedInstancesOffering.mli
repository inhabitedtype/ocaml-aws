open Types

type input = PurchaseReservedInstancesOfferingRequest.t

type output = PurchaseReservedInstancesOfferingResult.t

type error = Errors_internal.t

include
  Aws.Call with type input := input and type output := output and type error := error
