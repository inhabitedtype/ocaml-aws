open Types
type input = CreateCapacityReservationRequest.t
type output = CreateCapacityReservationResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error