open Types
type input = DescribePendingMaintenanceActionsMessage.t
type output = PendingMaintenanceActionsMessage.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error