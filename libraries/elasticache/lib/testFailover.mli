open Types
type input = TestFailoverMessage.t
type output = TestFailoverResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error