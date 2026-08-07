open Aws.BaseTypes

type t =
  { availability_zone : String.t option
  ; availability_zone_id : String.t option
  ; affinity : String.t option
  ; group_name : String.t option
  ; host_id : String.t option
  ; tenancy : Tenancy.t option
  ; spread_domain : String.t option
  ; host_resource_group_arn : String.t option
  ; partition_number : Integer.t option
  ; group_id : String.t option
  }

let make
    ?availability_zone
    ?availability_zone_id
    ?affinity
    ?group_name
    ?host_id
    ?tenancy
    ?spread_domain
    ?host_resource_group_arn
    ?partition_number
    ?group_id
    () =
  { availability_zone
  ; availability_zone_id
  ; affinity
  ; group_name
  ; host_id
  ; tenancy
  ; spread_domain
  ; host_resource_group_arn
  ; partition_number
  ; group_id
  }

let parse xml =
  Some
    { availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZoneId" xml) String.parse
    ; affinity = Aws.Util.option_bind (Aws.Xml.member "Affinity" xml) String.parse
    ; group_name = Aws.Util.option_bind (Aws.Xml.member "GroupName" xml) String.parse
    ; host_id = Aws.Util.option_bind (Aws.Xml.member "HostId" xml) String.parse
    ; tenancy = Aws.Util.option_bind (Aws.Xml.member "Tenancy" xml) Tenancy.parse
    ; spread_domain =
        Aws.Util.option_bind (Aws.Xml.member "SpreadDomain" xml) String.parse
    ; host_resource_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "HostResourceGroupArn" xml) String.parse
    ; partition_number =
        Aws.Util.option_bind (Aws.Xml.member "PartitionNumber" xml) Integer.parse
    ; group_id = Aws.Util.option_bind (Aws.Xml.member "GroupId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.group_id (fun f ->
             Aws.Query.Pair ("GroupId", String.to_query f))
       ; Aws.Util.option_map v.partition_number (fun f ->
             Aws.Query.Pair ("PartitionNumber", Integer.to_query f))
       ; Aws.Util.option_map v.host_resource_group_arn (fun f ->
             Aws.Query.Pair ("HostResourceGroupArn", String.to_query f))
       ; Aws.Util.option_map v.spread_domain (fun f ->
             Aws.Query.Pair ("SpreadDomain", String.to_query f))
       ; Aws.Util.option_map v.tenancy (fun f ->
             Aws.Query.Pair ("Tenancy", Tenancy.to_query f))
       ; Aws.Util.option_map v.host_id (fun f ->
             Aws.Query.Pair ("HostId", String.to_query f))
       ; Aws.Util.option_map v.group_name (fun f ->
             Aws.Query.Pair ("GroupName", String.to_query f))
       ; Aws.Util.option_map v.affinity (fun f ->
             Aws.Query.Pair ("Affinity", String.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.group_id (fun f -> "GroupId", String.to_json f)
       ; Aws.Util.option_map v.partition_number (fun f ->
             "PartitionNumber", Integer.to_json f)
       ; Aws.Util.option_map v.host_resource_group_arn (fun f ->
             "HostResourceGroupArn", String.to_json f)
       ; Aws.Util.option_map v.spread_domain (fun f -> "SpreadDomain", String.to_json f)
       ; Aws.Util.option_map v.tenancy (fun f -> "Tenancy", Tenancy.to_json f)
       ; Aws.Util.option_map v.host_id (fun f -> "HostId", String.to_json f)
       ; Aws.Util.option_map v.group_name (fun f -> "GroupName", String.to_json f)
       ; Aws.Util.option_map v.affinity (fun f -> "Affinity", String.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "AvailabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "AvailabilityZone", String.to_json f)
       ])

let of_json j =
  { availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZone") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZoneId") String.of_json
  ; affinity = Aws.Util.option_map (Aws.Json.lookup j "Affinity") String.of_json
  ; group_name = Aws.Util.option_map (Aws.Json.lookup j "GroupName") String.of_json
  ; host_id = Aws.Util.option_map (Aws.Json.lookup j "HostId") String.of_json
  ; tenancy = Aws.Util.option_map (Aws.Json.lookup j "Tenancy") Tenancy.of_json
  ; spread_domain = Aws.Util.option_map (Aws.Json.lookup j "SpreadDomain") String.of_json
  ; host_resource_group_arn =
      Aws.Util.option_map (Aws.Json.lookup j "HostResourceGroupArn") String.of_json
  ; partition_number =
      Aws.Util.option_map (Aws.Json.lookup j "PartitionNumber") Integer.of_json
  ; group_id = Aws.Util.option_map (Aws.Json.lookup j "GroupId") String.of_json
  }
