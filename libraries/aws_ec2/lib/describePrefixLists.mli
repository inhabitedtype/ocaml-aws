open Types
type input = DescribePrefixListsRequest.t
type output = DescribePrefixListsResult.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)