open Aws.BaseTypes

type t =
  { cache_node_type : String.t option
  ; value : String.t option
  }

let make ?cache_node_type ?value () = { cache_node_type; value }

let parse xml =
  Some
    { cache_node_type =
        Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
    ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.value (fun f ->
             Aws.Query.Pair ("Value", String.to_query f))
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             Aws.Query.Pair ("CacheNodeType", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.value (fun f -> "Value", String.to_json f)
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             "CacheNodeType", String.to_json f)
       ])

let of_json j =
  { cache_node_type =
      Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
  ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json
  }
