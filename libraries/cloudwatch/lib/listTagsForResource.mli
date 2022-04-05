open Types
type input = ListTagsForResourceInput.t
type output = ListTagsForResourceOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error