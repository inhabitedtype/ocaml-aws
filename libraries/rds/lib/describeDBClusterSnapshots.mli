open Types
type input = DescribeDBClusterSnapshotsMessage.t
type output = DBClusterSnapshotMessage.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)