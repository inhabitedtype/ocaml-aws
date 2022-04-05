open Types
type input = DescribeTrafficMirrorTargetsRequest.t
type output = DescribeTrafficMirrorTargetsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error