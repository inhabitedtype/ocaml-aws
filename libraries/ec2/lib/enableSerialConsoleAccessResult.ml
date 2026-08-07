open Aws.BaseTypes

type t = { serial_console_access_enabled : Boolean.t option }

let make ?serial_console_access_enabled () = { serial_console_access_enabled }

let parse xml =
  Some
    { serial_console_access_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "serialConsoleAccessEnabled" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.serial_console_access_enabled (fun f ->
             Aws.Query.Pair ("SerialConsoleAccessEnabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.serial_console_access_enabled (fun f ->
             "serialConsoleAccessEnabled", Boolean.to_json f)
       ])

let of_json j =
  { serial_console_access_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "serialConsoleAccessEnabled") Boolean.of_json
  }
