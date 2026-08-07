open Aws.BaseTypes

type t =
  { custom_key_store_id : String.t option
  ; custom_key_store_name : String.t option
  ; limit : Integer.t option
  ; marker : String.t option
  }

let make ?custom_key_store_id ?custom_key_store_name ?limit ?marker () =
  { custom_key_store_id; custom_key_store_name; limit; marker }

let parse xml =
  Some
    { custom_key_store_id =
        Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreId" xml) String.parse
    ; custom_key_store_name =
        Aws.Util.option_bind (Aws.Xml.member "CustomKeyStoreName" xml) String.parse
    ; limit = Aws.Util.option_bind (Aws.Xml.member "Limit" xml) Integer.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.limit (fun f ->
             Aws.Query.Pair ("Limit", Integer.to_query f))
       ; Aws.Util.option_map v.custom_key_store_name (fun f ->
             Aws.Query.Pair ("CustomKeyStoreName", String.to_query f))
       ; Aws.Util.option_map v.custom_key_store_id (fun f ->
             Aws.Query.Pair ("CustomKeyStoreId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.limit (fun f -> "Limit", Integer.to_json f)
       ; Aws.Util.option_map v.custom_key_store_name (fun f ->
             "CustomKeyStoreName", String.to_json f)
       ; Aws.Util.option_map v.custom_key_store_id (fun f ->
             "CustomKeyStoreId", String.to_json f)
       ])

let of_json j =
  { custom_key_store_id =
      Aws.Util.option_map (Aws.Json.lookup j "CustomKeyStoreId") String.of_json
  ; custom_key_store_name =
      Aws.Util.option_map (Aws.Json.lookup j "CustomKeyStoreName") String.of_json
  ; limit = Aws.Util.option_map (Aws.Json.lookup j "Limit") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
