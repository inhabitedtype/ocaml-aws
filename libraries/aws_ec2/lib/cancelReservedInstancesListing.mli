open Types
type input = CancelReservedInstancesListingRequest.t
type output = CancelReservedInstancesListingResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)