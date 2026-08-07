open Aws.BaseTypes

type t =
  { availability_zone_id : String.t option
  ; affinity : String.t option
  ; group_name : String.t option
  ; partition_number : Integer.t option
  ; host_id : String.t option
  ; tenancy : Tenancy.t option
  ; spread_domain : String.t option
  ; host_resource_group_arn : String.t option
  ; group_id : String.t option
  ; availability_zone : String.t option
  }

let make
    ?availability_zone_id
    ?affinity
    ?group_name
    ?partition_number
    ?host_id
    ?tenancy
    ?spread_domain
    ?host_resource_group_arn
    ?group_id
    ?availability_zone
    () =
  { availability_zone_id
  ; affinity
  ; group_name
  ; partition_number
  ; host_id
  ; tenancy
  ; spread_domain
  ; host_resource_group_arn
  ; group_id
  ; availability_zone
  }

let parse xml =
  Some
    { availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; affinity = Aws.Util.option_bind (Aws.Xml.member "affinity" xml) String.parse
    ; group_name = Aws.Util.option_bind (Aws.Xml.member "groupName" xml) String.parse
    ; partition_number =
        Aws.Util.option_bind (Aws.Xml.member "partitionNumber" xml) Integer.parse
    ; host_id = Aws.Util.option_bind (Aws.Xml.member "hostId" xml) String.parse
    ; tenancy = Aws.Util.option_bind (Aws.Xml.member "tenancy" xml) Tenancy.parse
    ; spread_domain =
        Aws.Util.option_bind (Aws.Xml.member "spreadDomain" xml) String.parse
    ; host_resource_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "hostResourceGroupArn" xml) String.parse
    ; group_id = Aws.Util.option_bind (Aws.Xml.member "groupId" xml) String.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.group_id (fun f ->
             Aws.Query.Pair ("GroupId", String.to_query f))
       ; Aws.Util.option_map v.host_resource_group_arn (fun f ->
             Aws.Query.Pair ("HostResourceGroupArn", String.to_query f))
       ; Aws.Util.option_map v.spread_domain (fun f ->
             Aws.Query.Pair ("SpreadDomain", String.to_query f))
       ; Aws.Util.option_map v.tenancy (fun f ->
             Aws.Query.Pair ("Tenancy", Tenancy.to_query f))
       ; Aws.Util.option_map v.host_id (fun f ->
             Aws.Query.Pair ("HostId", String.to_query f))
       ; Aws.Util.option_map v.partition_number (fun f ->
             Aws.Query.Pair ("PartitionNumber", Integer.to_query f))
       ; Aws.Util.option_map v.group_name (fun f ->
             Aws.Query.Pair ("GroupName", String.to_query f))
       ; Aws.Util.option_map v.affinity (fun f ->
             Aws.Query.Pair ("Affinity", String.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.group_id (fun f -> "groupId", String.to_json f)
       ; Aws.Util.option_map v.host_resource_group_arn (fun f ->
             "hostResourceGroupArn", String.to_json f)
       ; Aws.Util.option_map v.spread_domain (fun f -> "spreadDomain", String.to_json f)
       ; Aws.Util.option_map v.tenancy (fun f -> "tenancy", Tenancy.to_json f)
       ; Aws.Util.option_map v.host_id (fun f -> "hostId", String.to_json f)
       ; Aws.Util.option_map v.partition_number (fun f ->
             "partitionNumber", Integer.to_json f)
       ; Aws.Util.option_map v.group_name (fun f -> "groupName", String.to_json f)
       ; Aws.Util.option_map v.affinity (fun f -> "affinity", String.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ])

let of_json j =
  { availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; affinity = Aws.Util.option_map (Aws.Json.lookup j "affinity") String.of_json
  ; group_name = Aws.Util.option_map (Aws.Json.lookup j "groupName") String.of_json
  ; partition_number =
      Aws.Util.option_map (Aws.Json.lookup j "partitionNumber") Integer.of_json
  ; host_id = Aws.Util.option_map (Aws.Json.lookup j "hostId") String.of_json
  ; tenancy = Aws.Util.option_map (Aws.Json.lookup j "tenancy") Tenancy.of_json
  ; spread_domain = Aws.Util.option_map (Aws.Json.lookup j "spreadDomain") String.of_json
  ; host_resource_group_arn =
      Aws.Util.option_map (Aws.Json.lookup j "hostResourceGroupArn") String.of_json
  ; group_id = Aws.Util.option_map (Aws.Json.lookup j "groupId") String.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  }
