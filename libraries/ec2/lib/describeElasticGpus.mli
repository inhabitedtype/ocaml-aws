open Types
type input = DescribeElasticGpusRequest.t
type output = DescribeElasticGpusResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error