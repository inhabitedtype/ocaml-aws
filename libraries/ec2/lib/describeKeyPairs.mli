open Types
type input = DescribeKeyPairsRequest.t
type output = DescribeKeyPairsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error