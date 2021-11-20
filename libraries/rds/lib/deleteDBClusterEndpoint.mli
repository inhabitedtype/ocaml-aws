open Types
type input = DeleteDBClusterEndpointMessage.t
type output = DBClusterEndpoint.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error