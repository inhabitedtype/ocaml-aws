open Aws.BaseTypes

type t =
  { category : Category.t option
  ; publisher_id : String.t option
  ; type_name_prefix : String.t option
  }

let make ?category ?publisher_id ?type_name_prefix () =
  { category; publisher_id; type_name_prefix }

let parse xml =
  Some
    { category = Aws.Util.option_bind (Aws.Xml.member "Category" xml) Category.parse
    ; publisher_id = Aws.Util.option_bind (Aws.Xml.member "PublisherId" xml) String.parse
    ; type_name_prefix =
        Aws.Util.option_bind (Aws.Xml.member "TypeNamePrefix" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_name_prefix (fun f ->
             Aws.Query.Pair ("TypeNamePrefix", String.to_query f))
       ; Aws.Util.option_map v.publisher_id (fun f ->
             Aws.Query.Pair ("PublisherId", String.to_query f))
       ; Aws.Util.option_map v.category (fun f ->
             Aws.Query.Pair ("Category", Category.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_name_prefix (fun f ->
             "TypeNamePrefix", String.to_json f)
       ; Aws.Util.option_map v.publisher_id (fun f -> "PublisherId", String.to_json f)
       ; Aws.Util.option_map v.category (fun f -> "Category", Category.to_json f)
       ])

let of_json j =
  { category = Aws.Util.option_map (Aws.Json.lookup j "Category") Category.of_json
  ; publisher_id = Aws.Util.option_map (Aws.Json.lookup j "PublisherId") String.of_json
  ; type_name_prefix =
      Aws.Util.option_map (Aws.Json.lookup j "TypeNamePrefix") String.of_json
  }
