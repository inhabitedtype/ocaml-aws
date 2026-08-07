open Aws.BaseTypes

type t =
  { user_group_id : String.t option
  ; max_records : Integer.t option
  ; marker : String.t option
  }

let make ?user_group_id ?max_records ?marker () = { user_group_id; max_records; marker }

let parse xml =
  Some
    { user_group_id = Aws.Util.option_bind (Aws.Xml.member "UserGroupId" xml) String.parse
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Aws.Util.option_map v.user_group_id (fun f ->
             Aws.Query.Pair ("UserGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.user_group_id (fun f -> "UserGroupId", String.to_json f)
       ])

let of_json j =
  { user_group_id = Aws.Util.option_map (Aws.Json.lookup j "UserGroupId") String.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
