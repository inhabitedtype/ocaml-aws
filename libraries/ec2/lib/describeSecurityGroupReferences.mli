open Types
type input = DescribeSecurityGroupReferencesRequest.t
type output = DescribeSecurityGroupReferencesResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error