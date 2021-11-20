open Types
type input = DescribeDBClusterBacktracksMessage.t
type output = DBClusterBacktrackMessage.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error