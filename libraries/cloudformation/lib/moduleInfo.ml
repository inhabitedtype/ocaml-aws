open Aws.BaseTypes

type t =
  { type_hierarchy : String.t option
  ; logical_id_hierarchy : String.t option
  }

let make ?type_hierarchy ?logical_id_hierarchy () =
  { type_hierarchy; logical_id_hierarchy }

let parse xml =
  Some
    { type_hierarchy =
        Aws.Util.option_bind (Aws.Xml.member "TypeHierarchy" xml) String.parse
    ; logical_id_hierarchy =
        Aws.Util.option_bind (Aws.Xml.member "LogicalIdHierarchy" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.logical_id_hierarchy (fun f ->
             Aws.Query.Pair ("LogicalIdHierarchy", String.to_query f))
       ; Aws.Util.option_map v.type_hierarchy (fun f ->
             Aws.Query.Pair ("TypeHierarchy", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.logical_id_hierarchy (fun f ->
             "LogicalIdHierarchy", String.to_json f)
       ; Aws.Util.option_map v.type_hierarchy (fun f -> "TypeHierarchy", String.to_json f)
       ])

let of_json j =
  { type_hierarchy =
      Aws.Util.option_map (Aws.Json.lookup j "TypeHierarchy") String.of_json
  ; logical_id_hierarchy =
      Aws.Util.option_map (Aws.Json.lookup j "LogicalIdHierarchy") String.of_json
  }
