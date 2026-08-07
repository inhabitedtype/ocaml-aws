open Aws.BaseTypes

type t =
  { size_in_g_b : Long.t option
  ; count : Integer.t option
  ; type_ : DiskType.t option
  }

let make ?size_in_g_b ?count ?type_ () = { size_in_g_b; count; type_ }

let parse xml =
  Some
    { size_in_g_b = Aws.Util.option_bind (Aws.Xml.member "sizeInGB" xml) Long.parse
    ; count = Aws.Util.option_bind (Aws.Xml.member "count" xml) Integer.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "type" xml) DiskType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", DiskType.to_query f))
       ; Aws.Util.option_map v.count (fun f ->
             Aws.Query.Pair ("Count", Integer.to_query f))
       ; Aws.Util.option_map v.size_in_g_b (fun f ->
             Aws.Query.Pair ("SizeInGB", Long.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_ (fun f -> "type", DiskType.to_json f)
       ; Aws.Util.option_map v.count (fun f -> "count", Integer.to_json f)
       ; Aws.Util.option_map v.size_in_g_b (fun f -> "sizeInGB", Long.to_json f)
       ])

let of_json j =
  { size_in_g_b = Aws.Util.option_map (Aws.Json.lookup j "sizeInGB") Long.of_json
  ; count = Aws.Util.option_map (Aws.Json.lookup j "count") Integer.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "type") DiskType.of_json
  }
