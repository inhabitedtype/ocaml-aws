open Aws.BaseTypes

type t =
  { global_replication_group_id : String.t option
  ; max_records : Integer.t option
  ; marker : String.t option
  ; show_member_info : Boolean.t option
  }

let make ?global_replication_group_id ?max_records ?marker ?show_member_info () =
  { global_replication_group_id; max_records; marker; show_member_info }

let parse xml =
  Some
    { global_replication_group_id =
        Aws.Util.option_bind (Aws.Xml.member "GlobalReplicationGroupId" xml) String.parse
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; show_member_info =
        Aws.Util.option_bind (Aws.Xml.member "ShowMemberInfo" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.show_member_info (fun f ->
             Aws.Query.Pair ("ShowMemberInfo", Boolean.to_query f))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Aws.Util.option_map v.global_replication_group_id (fun f ->
             Aws.Query.Pair ("GlobalReplicationGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.show_member_info (fun f ->
             "ShowMemberInfo", Boolean.to_json f)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.global_replication_group_id (fun f ->
             "GlobalReplicationGroupId", String.to_json f)
       ])

let of_json j =
  { global_replication_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "GlobalReplicationGroupId") String.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; show_member_info =
      Aws.Util.option_map (Aws.Json.lookup j "ShowMemberInfo") Boolean.of_json
  }
