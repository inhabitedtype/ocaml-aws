open Types
type input = GetObjectTorrentRequest.t
type output = GetObjectTorrentOutput.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error