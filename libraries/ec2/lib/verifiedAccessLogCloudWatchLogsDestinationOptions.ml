open Aws.BaseTypes

type t =
  { enabled : Boolean.t
  ; log_group : String.t option
  }

let make ~enabled ?log_group () = { enabled; log_group }

let parse xml =
  Some
    { enabled =
        Aws.Xml.required
          "Enabled"
          (Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse)
    ; log_group = Aws.Util.option_bind (Aws.Xml.member "LogGroup" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.log_group (fun f ->
             Aws.Query.Pair ("LogGroup", String.to_query f))
       ; Some (Aws.Query.Pair ("Enabled", Boolean.to_query v.enabled))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.log_group (fun f -> "LogGroup", String.to_json f)
       ; Some ("Enabled", Boolean.to_json v.enabled)
       ])

let of_json j =
  { enabled = Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Enabled"))
  ; log_group = Aws.Util.option_map (Aws.Json.lookup j "LogGroup") String.of_json
  }
