open Aws.BaseTypes

type t =
  { target_groups : TargetGroupList.t
  ; marker : String.t option
  }

let make ?(target_groups = []) ?marker () = { target_groups; marker }

let parse xml =
  Some
    { target_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "TargetGroups" xml) TargetGroupList.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TargetGroups.member", TargetGroupList.to_query v.target_groups))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some ("TargetGroups", TargetGroupList.to_json v.target_groups)
       ])

let of_json j =
  { target_groups =
      TargetGroupList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetGroups"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
