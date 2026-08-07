open Aws.BaseTypes

type t =
  { process_name : String.t option
  ; suspension_reason : String.t option
  }

let make ?process_name ?suspension_reason () = { process_name; suspension_reason }

let parse xml =
  Some
    { process_name = Aws.Util.option_bind (Aws.Xml.member "ProcessName" xml) String.parse
    ; suspension_reason =
        Aws.Util.option_bind (Aws.Xml.member "SuspensionReason" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.suspension_reason (fun f ->
             Aws.Query.Pair ("SuspensionReason", String.to_query f))
       ; Aws.Util.option_map v.process_name (fun f ->
             Aws.Query.Pair ("ProcessName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.suspension_reason (fun f ->
             "SuspensionReason", String.to_json f)
       ; Aws.Util.option_map v.process_name (fun f -> "ProcessName", String.to_json f)
       ])

let of_json j =
  { process_name = Aws.Util.option_map (Aws.Json.lookup j "ProcessName") String.of_json
  ; suspension_reason =
      Aws.Util.option_map (Aws.Json.lookup j "SuspensionReason") String.of_json
  }
