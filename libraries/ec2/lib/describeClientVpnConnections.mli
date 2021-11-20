open Types
type input = DescribeClientVpnConnectionsRequest.t
type output = DescribeClientVpnConnectionsResult.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error