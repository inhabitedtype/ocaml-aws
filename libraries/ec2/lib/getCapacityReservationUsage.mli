open Types
type input = GetCapacityReservationUsageRequest.t
type output = GetCapacityReservationUsageResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error