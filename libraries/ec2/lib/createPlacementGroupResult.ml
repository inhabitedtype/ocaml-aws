type t = { placement_group : PlacementGroup.t option }

let make ?placement_group () = { placement_group }

let parse xml =
  Some
    { placement_group =
        Aws.Util.option_bind (Aws.Xml.member "placementGroup" xml) PlacementGroup.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.placement_group (fun f ->
             Aws.Query.Pair ("PlacementGroup", PlacementGroup.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.placement_group (fun f ->
             "placementGroup", PlacementGroup.to_json f)
       ])

let of_json j =
  { placement_group =
      Aws.Util.option_map (Aws.Json.lookup j "placementGroup") PlacementGroup.of_json
  }
