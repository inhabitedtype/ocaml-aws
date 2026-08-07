type t = { placement_groups : PlacementGroupList.t }

let make ?(placement_groups = []) () = { placement_groups }

let parse xml =
  Some
    { placement_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "placementGroupSet" xml)
             PlacementGroupList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("PlacementGroupSet", PlacementGroupList.to_query v.placement_groups))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("placementGroupSet", PlacementGroupList.to_json v.placement_groups) ])

let of_json j =
  { placement_groups =
      PlacementGroupList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "placementGroupSet"))
  }
