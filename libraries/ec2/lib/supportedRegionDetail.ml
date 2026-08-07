open Aws.BaseTypes

type t =
  { region : String.t option
  ; service_state : String.t option
  }

let make ?region ?service_state () = { region; service_state }

let parse xml =
  Some
    { region = Aws.Util.option_bind (Aws.Xml.member "region" xml) String.parse
    ; service_state =
        Aws.Util.option_bind (Aws.Xml.member "serviceState" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.service_state (fun f ->
             Aws.Query.Pair ("ServiceState", String.to_query f))
       ; Aws.Util.option_map v.region (fun f ->
             Aws.Query.Pair ("Region", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.service_state (fun f -> "serviceState", String.to_json f)
       ; Aws.Util.option_map v.region (fun f -> "region", String.to_json f)
       ])

let of_json j =
  { region = Aws.Util.option_map (Aws.Json.lookup j "region") String.of_json
  ; service_state = Aws.Util.option_map (Aws.Json.lookup j "serviceState") String.of_json
  }
