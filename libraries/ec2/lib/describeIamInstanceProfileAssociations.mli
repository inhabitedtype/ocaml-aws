open Types
type input = DescribeIamInstanceProfileAssociationsRequest.t
type output = DescribeIamInstanceProfileAssociationsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error