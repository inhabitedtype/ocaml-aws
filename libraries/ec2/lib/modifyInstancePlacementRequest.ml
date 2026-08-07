open Aws.BaseTypes

type t =
  { group_name : String.t option
  ; partition_number : Integer.t option
  ; host_resource_group_arn : String.t option
  ; group_id : String.t option
  ; instance_id : String.t
  ; tenancy : HostTenancy.t option
  ; affinity : Affinity.t option
  ; host_id : String.t option
  }

let make
    ?group_name
    ?partition_number
    ?host_resource_group_arn
    ?group_id
    ~instance_id
    ?tenancy
    ?affinity
    ?host_id
    () =
  { group_name
  ; partition_number
  ; host_resource_group_arn
  ; group_id
  ; instance_id
  ; tenancy
  ; affinity
  ; host_id
  }

let parse xml =
  Some
    { group_name = Aws.Util.option_bind (Aws.Xml.member "GroupName" xml) String.parse
    ; partition_number =
        Aws.Util.option_bind (Aws.Xml.member "PartitionNumber" xml) Integer.parse
    ; host_resource_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "HostResourceGroupArn" xml) String.parse
    ; group_id = Aws.Util.option_bind (Aws.Xml.member "GroupId" xml) String.parse
    ; instance_id =
        Aws.Xml.required
          "instanceId"
          (Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse)
    ; tenancy = Aws.Util.option_bind (Aws.Xml.member "tenancy" xml) HostTenancy.parse
    ; affinity = Aws.Util.option_bind (Aws.Xml.member "affinity" xml) Affinity.parse
    ; host_id = Aws.Util.option_bind (Aws.Xml.member "hostId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.host_id (fun f ->
             Aws.Query.Pair ("HostId", String.to_query f))
       ; Aws.Util.option_map v.affinity (fun f ->
             Aws.Query.Pair ("Affinity", Affinity.to_query f))
       ; Aws.Util.option_map v.tenancy (fun f ->
             Aws.Query.Pair ("Tenancy", HostTenancy.to_query f))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ; Aws.Util.option_map v.group_id (fun f ->
             Aws.Query.Pair ("GroupId", String.to_query f))
       ; Aws.Util.option_map v.host_resource_group_arn (fun f ->
             Aws.Query.Pair ("HostResourceGroupArn", String.to_query f))
       ; Aws.Util.option_map v.partition_number (fun f ->
             Aws.Query.Pair ("PartitionNumber", Integer.to_query f))
       ; Aws.Util.option_map v.group_name (fun f ->
             Aws.Query.Pair ("GroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.host_id (fun f -> "hostId", String.to_json f)
       ; Aws.Util.option_map v.affinity (fun f -> "affinity", Affinity.to_json f)
       ; Aws.Util.option_map v.tenancy (fun f -> "tenancy", HostTenancy.to_json f)
       ; Some ("instanceId", String.to_json v.instance_id)
       ; Aws.Util.option_map v.group_id (fun f -> "GroupId", String.to_json f)
       ; Aws.Util.option_map v.host_resource_group_arn (fun f ->
             "HostResourceGroupArn", String.to_json f)
       ; Aws.Util.option_map v.partition_number (fun f ->
             "PartitionNumber", Integer.to_json f)
       ; Aws.Util.option_map v.group_name (fun f -> "GroupName", String.to_json f)
       ])

let of_json j =
  { group_name = Aws.Util.option_map (Aws.Json.lookup j "GroupName") String.of_json
  ; partition_number =
      Aws.Util.option_map (Aws.Json.lookup j "PartitionNumber") Integer.of_json
  ; host_resource_group_arn =
      Aws.Util.option_map (Aws.Json.lookup j "HostResourceGroupArn") String.of_json
  ; group_id = Aws.Util.option_map (Aws.Json.lookup j "GroupId") String.of_json
  ; instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceId"))
  ; tenancy = Aws.Util.option_map (Aws.Json.lookup j "tenancy") HostTenancy.of_json
  ; affinity = Aws.Util.option_map (Aws.Json.lookup j "affinity") Affinity.of_json
  ; host_id = Aws.Util.option_map (Aws.Json.lookup j "hostId") String.of_json
  }
