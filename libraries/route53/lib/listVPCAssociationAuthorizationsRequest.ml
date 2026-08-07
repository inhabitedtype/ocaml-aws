open Aws.BaseTypes

type t =
  { hosted_zone_id : String.t
  ; next_token : String.t option
  ; max_results : String.t option
  }

let make ~hosted_zone_id ?next_token ?max_results () =
  { hosted_zone_id; next_token; max_results }

let parse xml =
  Some
    { hosted_zone_id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
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
       ; Some (Aws.Query.Pair ("Id", String.to_query v.hosted_zone_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "maxresults", String.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "nexttoken", String.to_json f)
       ; Some ("Id", String.to_json v.hosted_zone_id)
       ])

let of_json j =
  { hosted_zone_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nexttoken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "maxresults") String.of_json
  }
