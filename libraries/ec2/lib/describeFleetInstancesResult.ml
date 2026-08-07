open Aws.BaseTypes

type t =
  { active_instances : ActiveInstanceSet.t
  ; next_token : String.t option
  ; fleet_id : String.t option
  }

let make ?(active_instances = []) ?next_token ?fleet_id () =
  { active_instances; next_token; fleet_id }

let parse xml =
  Some
    { active_instances =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "activeInstanceSet" xml)
             ActiveInstanceSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; fleet_id = Aws.Util.option_bind (Aws.Xml.member "fleetId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.fleet_id (fun f ->
             Aws.Query.Pair ("FleetId", String.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ActiveInstanceSet", ActiveInstanceSet.to_query v.active_instances))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.fleet_id (fun f -> "fleetId", String.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("activeInstanceSet", ActiveInstanceSet.to_json v.active_instances)
       ])

let of_json j =
  { active_instances =
      ActiveInstanceSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "activeInstanceSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; fleet_id = Aws.Util.option_map (Aws.Json.lookup j "fleetId") String.of_json
  }
