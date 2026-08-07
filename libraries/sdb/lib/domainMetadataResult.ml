open Aws.BaseTypes

type t =
  { item_count : Integer.t option
  ; item_names_size_bytes : Long.t option
  ; attribute_name_count : Integer.t option
  ; attribute_names_size_bytes : Long.t option
  ; attribute_value_count : Integer.t option
  ; attribute_values_size_bytes : Long.t option
  ; timestamp : Integer.t option
  }

let make
    ?item_count
    ?item_names_size_bytes
    ?attribute_name_count
    ?attribute_names_size_bytes
    ?attribute_value_count
    ?attribute_values_size_bytes
    ?timestamp
    () =
  { item_count
  ; item_names_size_bytes
  ; attribute_name_count
  ; attribute_names_size_bytes
  ; attribute_value_count
  ; attribute_values_size_bytes
  ; timestamp
  }

let parse xml =
  Some
    { item_count = Aws.Util.option_bind (Aws.Xml.member "ItemCount" xml) Integer.parse
    ; item_names_size_bytes =
        Aws.Util.option_bind (Aws.Xml.member "ItemNamesSizeBytes" xml) Long.parse
    ; attribute_name_count =
        Aws.Util.option_bind (Aws.Xml.member "AttributeNameCount" xml) Integer.parse
    ; attribute_names_size_bytes =
        Aws.Util.option_bind (Aws.Xml.member "AttributeNamesSizeBytes" xml) Long.parse
    ; attribute_value_count =
        Aws.Util.option_bind (Aws.Xml.member "AttributeValueCount" xml) Integer.parse
    ; attribute_values_size_bytes =
        Aws.Util.option_bind (Aws.Xml.member "AttributeValuesSizeBytes" xml) Long.parse
    ; timestamp = Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timestamp (fun f ->
             Aws.Query.Pair ("Timestamp", Integer.to_query f))
       ; Aws.Util.option_map v.attribute_values_size_bytes (fun f ->
             Aws.Query.Pair ("AttributeValuesSizeBytes", Long.to_query f))
       ; Aws.Util.option_map v.attribute_value_count (fun f ->
             Aws.Query.Pair ("AttributeValueCount", Integer.to_query f))
       ; Aws.Util.option_map v.attribute_names_size_bytes (fun f ->
             Aws.Query.Pair ("AttributeNamesSizeBytes", Long.to_query f))
       ; Aws.Util.option_map v.attribute_name_count (fun f ->
             Aws.Query.Pair ("AttributeNameCount", Integer.to_query f))
       ; Aws.Util.option_map v.item_names_size_bytes (fun f ->
             Aws.Query.Pair ("ItemNamesSizeBytes", Long.to_query f))
       ; Aws.Util.option_map v.item_count (fun f ->
             Aws.Query.Pair ("ItemCount", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timestamp (fun f -> "Timestamp", Integer.to_json f)
       ; Aws.Util.option_map v.attribute_values_size_bytes (fun f ->
             "AttributeValuesSizeBytes", Long.to_json f)
       ; Aws.Util.option_map v.attribute_value_count (fun f ->
             "AttributeValueCount", Integer.to_json f)
       ; Aws.Util.option_map v.attribute_names_size_bytes (fun f ->
             "AttributeNamesSizeBytes", Long.to_json f)
       ; Aws.Util.option_map v.attribute_name_count (fun f ->
             "AttributeNameCount", Integer.to_json f)
       ; Aws.Util.option_map v.item_names_size_bytes (fun f ->
             "ItemNamesSizeBytes", Long.to_json f)
       ; Aws.Util.option_map v.item_count (fun f -> "ItemCount", Integer.to_json f)
       ])

let of_json j =
  { item_count = Aws.Util.option_map (Aws.Json.lookup j "ItemCount") Integer.of_json
  ; item_names_size_bytes =
      Aws.Util.option_map (Aws.Json.lookup j "ItemNamesSizeBytes") Long.of_json
  ; attribute_name_count =
      Aws.Util.option_map (Aws.Json.lookup j "AttributeNameCount") Integer.of_json
  ; attribute_names_size_bytes =
      Aws.Util.option_map (Aws.Json.lookup j "AttributeNamesSizeBytes") Long.of_json
  ; attribute_value_count =
      Aws.Util.option_map (Aws.Json.lookup j "AttributeValueCount") Integer.of_json
  ; attribute_values_size_bytes =
      Aws.Util.option_map (Aws.Json.lookup j "AttributeValuesSizeBytes") Long.of_json
  ; timestamp = Aws.Util.option_map (Aws.Json.lookup j "Timestamp") Integer.of_json
  }
