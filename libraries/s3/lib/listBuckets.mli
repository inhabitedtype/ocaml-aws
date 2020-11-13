open Types
type input = Aws.BaseTypes.Unit.t
type output = ListBucketsOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error