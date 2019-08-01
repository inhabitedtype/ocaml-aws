open Types
type input = RunInstancesRequest.t
type output = Reservation.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error