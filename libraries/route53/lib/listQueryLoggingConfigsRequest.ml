open Aws.BaseTypes

type t =
  { hosted_zone_id : String.t option
  ; next_token : String.t option
  ; max_results : String.t option
  }

let make ?hosted_zone_id ?next_token ?max_results () =
  { hosted_zone_id; next_token; max_results }

let parse xml =
  Some
    { hosted_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "hostedzoneid" xml) String.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nexttoken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "maxresults" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("maxresults", String.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("nexttoken", String.to_query f))
       ; Aws.Util.option_map v.hosted_zone_id (fun f ->
             Aws.Query.Pair ("hostedzoneid", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "maxresults", String.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "nexttoken", String.to_json f)
       ; Aws.Util.option_map v.hosted_zone_id (fun f -> "hostedzoneid", String.to_json f)
       ])

let of_json j =
  { hosted_zone_id = Aws.Util.option_map (Aws.Json.lookup j "hostedzoneid") String.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nexttoken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "maxresults") String.of_json
  }
