open Aws.BaseTypes

type t =
  { d_n_s_name : String.t option
  ; hosted_zone_id : String.t option
  ; max_items : String.t option
  }

let make ?d_n_s_name ?hosted_zone_id ?max_items () =
  { d_n_s_name; hosted_zone_id; max_items }

let parse xml =
  Some
    { d_n_s_name = Aws.Util.option_bind (Aws.Xml.member "dnsname" xml) String.parse
    ; hosted_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "hostedzoneid" xml) String.parse
    ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_items (fun f ->
             Aws.Query.Pair ("maxitems", String.to_query f))
       ; Aws.Util.option_map v.hosted_zone_id (fun f ->
             Aws.Query.Pair ("hostedzoneid", String.to_query f))
       ; Aws.Util.option_map v.d_n_s_name (fun f ->
             Aws.Query.Pair ("dnsname", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
       ; Aws.Util.option_map v.hosted_zone_id (fun f -> "hostedzoneid", String.to_json f)
       ; Aws.Util.option_map v.d_n_s_name (fun f -> "dnsname", String.to_json f)
       ])

let of_json j =
  { d_n_s_name = Aws.Util.option_map (Aws.Json.lookup j "dnsname") String.of_json
  ; hosted_zone_id = Aws.Util.option_map (Aws.Json.lookup j "hostedzoneid") String.of_json
  ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
  }
