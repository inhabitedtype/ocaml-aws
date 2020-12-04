open Types
type input = ImportInstallationMediaMessage.t
type output = InstallationMedia.t
type error = Errors_internal.t
include
  Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error