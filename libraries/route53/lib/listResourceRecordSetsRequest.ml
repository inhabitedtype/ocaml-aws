open Aws.BaseTypes

type t =
  { hosted_zone_id : String.t
  ; start_record_name : String.t option
  ; start_record_type : RRType.t option
  ; start_record_identifier : String.t option
  ; max_items : String.t option
  }

let make
    ~hosted_zone_id
    ?start_record_name
    ?start_record_type
    ?start_record_identifier
    ?max_items
    () =
  { hosted_zone_id
  ; start_record_name
  ; start_record_type
  ; start_record_identifier
  ; max_items
  }

let parse xml =
  Some
    { hosted_zone_id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
    ; start_record_name = Aws.Util.option_bind (Aws.Xml.member "name" xml) String.parse
    ; start_record_type = Aws.Util.option_bind (Aws.Xml.member "type" xml) RRType.parse
    ; start_record_identifier =
        Aws.Util.option_bind (Aws.Xml.member "identifier" xml) String.parse
    ; max_items = Aws.Util.option_bind (Aws.Xml.member "maxitems" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_items (fun f ->
             Aws.Query.Pair ("maxitems", String.to_query f))
       ; Aws.Util.option_map v.start_record_identifier (fun f ->
             Aws.Query.Pair ("identifier", String.to_query f))
       ; Aws.Util.option_map v.start_record_type (fun f ->
             Aws.Query.Pair ("type", RRType.to_query f))
       ; Aws.Util.option_map v.start_record_name (fun f ->
             Aws.Query.Pair ("name", String.to_query f))
       ; Some (Aws.Query.Pair ("Id", String.to_query v.hosted_zone_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_items (fun f -> "maxitems", String.to_json f)
       ; Aws.Util.option_map v.start_record_identifier (fun f ->
             "identifier", String.to_json f)
       ; Aws.Util.option_map v.start_record_type (fun f -> "type", RRType.to_json f)
       ; Aws.Util.option_map v.start_record_name (fun f -> "name", String.to_json f)
       ; Some ("Id", String.to_json v.hosted_zone_id)
       ])

let of_json j =
  { hosted_zone_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; start_record_name = Aws.Util.option_map (Aws.Json.lookup j "name") String.of_json
  ; start_record_type = Aws.Util.option_map (Aws.Json.lookup j "type") RRType.of_json
  ; start_record_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "identifier") String.of_json
  ; max_items = Aws.Util.option_map (Aws.Json.lookup j "maxitems") String.of_json
  }
