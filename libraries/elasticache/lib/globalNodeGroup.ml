open Aws.BaseTypes

type t =
  { global_node_group_id : String.t option
  ; slots : String.t option
  }

let make ?global_node_group_id ?slots () = { global_node_group_id; slots }

let parse xml =
  Some
    { global_node_group_id =
        Aws.Util.option_bind (Aws.Xml.member "GlobalNodeGroupId" xml) String.parse
    ; slots = Aws.Util.option_bind (Aws.Xml.member "Slots" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.slots (fun f ->
             Aws.Query.Pair ("Slots", String.to_query f))
       ; Aws.Util.option_map v.global_node_group_id (fun f ->
             Aws.Query.Pair ("GlobalNodeGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.slots (fun f -> "Slots", String.to_json f)
       ; Aws.Util.option_map v.global_node_group_id (fun f ->
             "GlobalNodeGroupId", String.to_json f)
       ])

let of_json j =
  { global_node_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "GlobalNodeGroupId") String.of_json
  ; slots = Aws.Util.option_map (Aws.Json.lookup j "Slots") String.of_json
  }
