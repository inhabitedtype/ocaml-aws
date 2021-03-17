open Types
type input = CreateChangeSetInput.t
type output = CreateChangeSetOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error