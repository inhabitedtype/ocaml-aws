open Aws.BaseTypes

type t =
  { fleet_id : String.t option
  ; errors : CreateFleetErrorsSet.t
  ; instances : CreateFleetInstancesSet.t
  }

let make ?fleet_id ?(errors = []) ?(instances = []) () = { fleet_id; errors; instances }

let parse xml =
  Some
    { fleet_id = Aws.Util.option_bind (Aws.Xml.member "fleetId" xml) String.parse
    ; errors =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "errorSet" xml)
             CreateFleetErrorsSet.parse)
    ; instances =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "fleetInstanceSet" xml)
             CreateFleetInstancesSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("FleetInstanceSet", CreateFleetInstancesSet.to_query v.instances))
       ; Some (Aws.Query.Pair ("ErrorSet", CreateFleetErrorsSet.to_query v.errors))
       ; Aws.Util.option_map v.fleet_id (fun f ->
             Aws.Query.Pair ("FleetId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("fleetInstanceSet", CreateFleetInstancesSet.to_json v.instances)
       ; Some ("errorSet", CreateFleetErrorsSet.to_json v.errors)
       ; Aws.Util.option_map v.fleet_id (fun f -> "fleetId", String.to_json f)
       ])

let of_json j =
  { fleet_id = Aws.Util.option_map (Aws.Json.lookup j "fleetId") String.of_json
  ; errors =
      CreateFleetErrorsSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "errorSet"))
  ; instances =
      CreateFleetInstancesSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "fleetInstanceSet"))
  }
