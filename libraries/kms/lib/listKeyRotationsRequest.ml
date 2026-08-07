open Aws.BaseTypes

type t =
  { key_id : String.t
  ; include_key_material : IncludeKeyMaterial.t option
  ; limit : Integer.t option
  ; marker : String.t option
  }

let make ~key_id ?include_key_material ?limit ?marker () =
  { key_id; include_key_material; limit; marker }

let parse xml =
  Some
    { key_id =
        Aws.Xml.required
          "KeyId"
          (Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse)
    ; include_key_material =
        Aws.Util.option_bind
          (Aws.Xml.member "IncludeKeyMaterial" xml)
          IncludeKeyMaterial.parse
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
       ; Aws.Util.option_map v.include_key_material (fun f ->
             Aws.Query.Pair ("IncludeKeyMaterial", IncludeKeyMaterial.to_query f))
       ; Some (Aws.Query.Pair ("KeyId", String.to_query v.key_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.limit (fun f -> "Limit", Integer.to_json f)
       ; Aws.Util.option_map v.include_key_material (fun f ->
             "IncludeKeyMaterial", IncludeKeyMaterial.to_json f)
       ; Some ("KeyId", String.to_json v.key_id)
       ])

let of_json j =
  { key_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "KeyId"))
  ; include_key_material =
      Aws.Util.option_map
        (Aws.Json.lookup j "IncludeKeyMaterial")
        IncludeKeyMaterial.of_json
  ; limit = Aws.Util.option_map (Aws.Json.lookup j "Limit") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
