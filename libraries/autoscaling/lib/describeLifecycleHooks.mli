open Types
type input = DescribeLifecycleHooksType.t
type output = DescribeLifecycleHooksAnswer.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error