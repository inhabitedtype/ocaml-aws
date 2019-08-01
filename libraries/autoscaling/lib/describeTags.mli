open Types
type input = DescribeTagsType.t
type output = TagsType.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error