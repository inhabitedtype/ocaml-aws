open Types
type input = DescribeVpcClassicLinkRequest.t
type output = DescribeVpcClassicLinkResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)