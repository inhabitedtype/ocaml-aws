open Types
type input = ModifyDBSnapshotAttributeMessage.t
type output = ModifyDBSnapshotAttributeResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error