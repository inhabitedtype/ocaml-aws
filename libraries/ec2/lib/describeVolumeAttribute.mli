open Types
type input = DescribeVolumeAttributeRequest.t
type output = DescribeVolumeAttributeResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error