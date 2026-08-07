open Aws.BaseTypes

type t =
  { enabled : Boolean.t
  ; timeout : Integer.t option
  }

let make ~enabled ?timeout () = { enabled; timeout }

let parse xml =
  Some
    { enabled =
        Aws.Xml.required
          "Enabled"
          (Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse)
    ; timeout = Aws.Util.option_bind (Aws.Xml.member "Timeout" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timeout (fun f ->
             Aws.Query.Pair ("Timeout", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Enabled", Boolean.to_query v.enabled))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timeout (fun f -> "Timeout", Integer.to_json f)
       ; Some ("Enabled", Boolean.to_json v.enabled)
       ])

let of_json j =
  { enabled = Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Enabled"))
  ; timeout = Aws.Util.option_map (Aws.Json.lookup j "Timeout") Integer.of_json
  }
