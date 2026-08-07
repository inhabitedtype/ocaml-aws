open Aws.BaseTypes

type t =
  { serial_console_access_enabled : Boolean.t option
  ; managed_by : ManagedBy.t option
  }

let make ?serial_console_access_enabled ?managed_by () =
  { serial_console_access_enabled; managed_by }

let parse xml =
  Some
    { serial_console_access_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "serialConsoleAccessEnabled" xml)
          Boolean.parse
    ; managed_by = Aws.Util.option_bind (Aws.Xml.member "managedBy" xml) ManagedBy.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.managed_by (fun f ->
             Aws.Query.Pair ("ManagedBy", ManagedBy.to_query f))
       ; Aws.Util.option_map v.serial_console_access_enabled (fun f ->
             Aws.Query.Pair ("SerialConsoleAccessEnabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.managed_by (fun f -> "managedBy", ManagedBy.to_json f)
       ; Aws.Util.option_map v.serial_console_access_enabled (fun f ->
             "serialConsoleAccessEnabled", Boolean.to_json f)
       ])

let of_json j =
  { serial_console_access_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "serialConsoleAccessEnabled") Boolean.of_json
  ; managed_by = Aws.Util.option_map (Aws.Json.lookup j "managedBy") ManagedBy.of_json
  }
