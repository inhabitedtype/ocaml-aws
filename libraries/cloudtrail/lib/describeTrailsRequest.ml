open Aws.BaseTypes

type t =
  { trail_name_list : TrailNameList.t
  ; include_shadow_trails : Boolean.t option
  }

let make ?(trail_name_list = []) ?include_shadow_trails () =
  { trail_name_list; include_shadow_trails }

let parse xml =
  Some
    { trail_name_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "trailNameList" xml) TrailNameList.parse)
    ; include_shadow_trails =
        Aws.Util.option_bind (Aws.Xml.member "includeShadowTrails" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_shadow_trails (fun f ->
             Aws.Query.Pair ("includeShadowTrails", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("trailNameList.member", TrailNameList.to_query v.trail_name_list))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.include_shadow_trails (fun f ->
             "includeShadowTrails", Boolean.to_json f)
       ; Some ("trailNameList", TrailNameList.to_json v.trail_name_list)
       ])

let of_json j =
  { trail_name_list =
      TrailNameList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "trailNameList"))
  ; include_shadow_trails =
      Aws.Util.option_map (Aws.Json.lookup j "includeShadowTrails") Boolean.of_json
  }
