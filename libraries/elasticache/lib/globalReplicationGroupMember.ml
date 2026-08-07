open Aws.BaseTypes

type t =
  { replication_group_id : String.t option
  ; replication_group_region : String.t option
  ; role : String.t option
  ; automatic_failover : AutomaticFailoverStatus.t option
  ; status : String.t option
  }

let make
    ?replication_group_id
    ?replication_group_region
    ?role
    ?automatic_failover
    ?status
    () =
  { replication_group_id; replication_group_region; role; automatic_failover; status }

let parse xml =
  Some
    { replication_group_id =
        Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
    ; replication_group_region =
        Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupRegion" xml) String.parse
    ; role = Aws.Util.option_bind (Aws.Xml.member "Role" xml) String.parse
    ; automatic_failover =
        Aws.Util.option_bind
          (Aws.Xml.member "AutomaticFailover" xml)
          AutomaticFailoverStatus.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.automatic_failover (fun f ->
             Aws.Query.Pair ("AutomaticFailover", AutomaticFailoverStatus.to_query f))
       ; Aws.Util.option_map v.role (fun f -> Aws.Query.Pair ("Role", String.to_query f))
       ; Aws.Util.option_map v.replication_group_region (fun f ->
             Aws.Query.Pair ("ReplicationGroupRegion", String.to_query f))
       ; Aws.Util.option_map v.replication_group_id (fun f ->
             Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.automatic_failover (fun f ->
             "AutomaticFailover", AutomaticFailoverStatus.to_json f)
       ; Aws.Util.option_map v.role (fun f -> "Role", String.to_json f)
       ; Aws.Util.option_map v.replication_group_region (fun f ->
             "ReplicationGroupRegion", String.to_json f)
       ; Aws.Util.option_map v.replication_group_id (fun f ->
             "ReplicationGroupId", String.to_json f)
       ])

let of_json j =
  { replication_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
  ; replication_group_region =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupRegion") String.of_json
  ; role = Aws.Util.option_map (Aws.Json.lookup j "Role") String.of_json
  ; automatic_failover =
      Aws.Util.option_map
        (Aws.Json.lookup j "AutomaticFailover")
        AutomaticFailoverStatus.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  }
