open Aws.BaseTypes

type t =
  { client_token : String.t option
  ; instance_type : String.t
  ; instance_platform : CapacityReservationInstancePlatform.t
  ; availability_zone : String.t option
  ; availability_zone_id : String.t option
  ; tenancy : CapacityReservationTenancy.t option
  ; instance_count : Integer.t
  ; ebs_optimized : Boolean.t option
  ; ephemeral_storage : Boolean.t option
  ; end_date : DateTime.t option
  ; end_date_type : EndDateType.t option
  ; instance_match_criteria : InstanceMatchCriteria.t option
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  ; outpost_arn : String.t option
  ; placement_group_arn : String.t option
  ; start_date : DateTime.t option
  ; commitment_duration : Long.t option
  ; delivery_preference : CapacityReservationDeliveryPreference.t option
  }

let make
    ?client_token
    ~instance_type
    ~instance_platform
    ?availability_zone
    ?availability_zone_id
    ?tenancy
    ~instance_count
    ?ebs_optimized
    ?ephemeral_storage
    ?end_date
    ?end_date_type
    ?instance_match_criteria
    ?(tag_specifications = [])
    ?dry_run
    ?outpost_arn
    ?placement_group_arn
    ?start_date
    ?commitment_duration
    ?delivery_preference
    () =
  { client_token
  ; instance_type
  ; instance_platform
  ; availability_zone
  ; availability_zone_id
  ; tenancy
  ; instance_count
  ; ebs_optimized
  ; ephemeral_storage
  ; end_date
  ; end_date_type
  ; instance_match_criteria
  ; tag_specifications
  ; dry_run
  ; outpost_arn
  ; placement_group_arn
  ; start_date
  ; commitment_duration
  ; delivery_preference
  }

let parse xml =
  Some
    { client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; instance_type =
        Aws.Xml.required
          "InstanceType"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceType" xml) String.parse)
    ; instance_platform =
        Aws.Xml.required
          "InstancePlatform"
          (Aws.Util.option_bind
             (Aws.Xml.member "InstancePlatform" xml)
             CapacityReservationInstancePlatform.parse)
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZoneId" xml) String.parse
    ; tenancy =
        Aws.Util.option_bind
          (Aws.Xml.member "Tenancy" xml)
          CapacityReservationTenancy.parse
    ; instance_count =
        Aws.Xml.required
          "InstanceCount"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceCount" xml) Integer.parse)
    ; ebs_optimized =
        Aws.Util.option_bind (Aws.Xml.member "EbsOptimized" xml) Boolean.parse
    ; ephemeral_storage =
        Aws.Util.option_bind (Aws.Xml.member "EphemeralStorage" xml) Boolean.parse
    ; end_date = Aws.Util.option_bind (Aws.Xml.member "EndDate" xml) DateTime.parse
    ; end_date_type =
        Aws.Util.option_bind (Aws.Xml.member "EndDateType" xml) EndDateType.parse
    ; instance_match_criteria =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceMatchCriteria" xml)
          InstanceMatchCriteria.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecifications" xml)
             TagSpecificationList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "OutpostArn" xml) String.parse
    ; placement_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "PlacementGroupArn" xml) String.parse
    ; start_date = Aws.Util.option_bind (Aws.Xml.member "StartDate" xml) DateTime.parse
    ; commitment_duration =
        Aws.Util.option_bind (Aws.Xml.member "CommitmentDuration" xml) Long.parse
    ; delivery_preference =
        Aws.Util.option_bind
          (Aws.Xml.member "DeliveryPreference" xml)
          CapacityReservationDeliveryPreference.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delivery_preference (fun f ->
             Aws.Query.Pair
               ("DeliveryPreference", CapacityReservationDeliveryPreference.to_query f))
       ; Aws.Util.option_map v.commitment_duration (fun f ->
             Aws.Query.Pair ("CommitmentDuration", Long.to_query f))
       ; Aws.Util.option_map v.start_date (fun f ->
             Aws.Query.Pair ("StartDate", DateTime.to_query f))
       ; Aws.Util.option_map v.placement_group_arn (fun f ->
             Aws.Query.Pair ("PlacementGroupArn", String.to_query f))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecifications", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.instance_match_criteria (fun f ->
             Aws.Query.Pair ("InstanceMatchCriteria", InstanceMatchCriteria.to_query f))
       ; Aws.Util.option_map v.end_date_type (fun f ->
             Aws.Query.Pair ("EndDateType", EndDateType.to_query f))
       ; Aws.Util.option_map v.end_date (fun f ->
             Aws.Query.Pair ("EndDate", DateTime.to_query f))
       ; Aws.Util.option_map v.ephemeral_storage (fun f ->
             Aws.Query.Pair ("EphemeralStorage", Boolean.to_query f))
       ; Aws.Util.option_map v.ebs_optimized (fun f ->
             Aws.Query.Pair ("EbsOptimized", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("InstanceCount", Integer.to_query v.instance_count))
       ; Aws.Util.option_map v.tenancy (fun f ->
             Aws.Query.Pair ("Tenancy", CapacityReservationTenancy.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "InstancePlatform"
              , CapacityReservationInstancePlatform.to_query v.instance_platform ))
       ; Some (Aws.Query.Pair ("InstanceType", String.to_query v.instance_type))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delivery_preference (fun f ->
             "DeliveryPreference", CapacityReservationDeliveryPreference.to_json f)
       ; Aws.Util.option_map v.commitment_duration (fun f ->
             "CommitmentDuration", Long.to_json f)
       ; Aws.Util.option_map v.start_date (fun f -> "StartDate", DateTime.to_json f)
       ; Aws.Util.option_map v.placement_group_arn (fun f ->
             "PlacementGroupArn", String.to_json f)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "OutpostArn", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecifications", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.instance_match_criteria (fun f ->
             "InstanceMatchCriteria", InstanceMatchCriteria.to_json f)
       ; Aws.Util.option_map v.end_date_type (fun f ->
             "EndDateType", EndDateType.to_json f)
       ; Aws.Util.option_map v.end_date (fun f -> "EndDate", DateTime.to_json f)
       ; Aws.Util.option_map v.ephemeral_storage (fun f ->
             "EphemeralStorage", Boolean.to_json f)
       ; Aws.Util.option_map v.ebs_optimized (fun f -> "EbsOptimized", Boolean.to_json f)
       ; Some ("InstanceCount", Integer.to_json v.instance_count)
       ; Aws.Util.option_map v.tenancy (fun f ->
             "Tenancy", CapacityReservationTenancy.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "AvailabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "AvailabilityZone", String.to_json f)
       ; Some
           ( "InstancePlatform"
           , CapacityReservationInstancePlatform.to_json v.instance_platform )
       ; Some ("InstanceType", String.to_json v.instance_type)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ])

let of_json j =
  { client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; instance_type =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceType"))
  ; instance_platform =
      CapacityReservationInstancePlatform.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstancePlatform"))
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZone") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZoneId") String.of_json
  ; tenancy =
      Aws.Util.option_map (Aws.Json.lookup j "Tenancy") CapacityReservationTenancy.of_json
  ; instance_count =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceCount"))
  ; ebs_optimized = Aws.Util.option_map (Aws.Json.lookup j "EbsOptimized") Boolean.of_json
  ; ephemeral_storage =
      Aws.Util.option_map (Aws.Json.lookup j "EphemeralStorage") Boolean.of_json
  ; end_date = Aws.Util.option_map (Aws.Json.lookup j "EndDate") DateTime.of_json
  ; end_date_type =
      Aws.Util.option_map (Aws.Json.lookup j "EndDateType") EndDateType.of_json
  ; instance_match_criteria =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceMatchCriteria")
        InstanceMatchCriteria.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecifications"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "OutpostArn") String.of_json
  ; placement_group_arn =
      Aws.Util.option_map (Aws.Json.lookup j "PlacementGroupArn") String.of_json
  ; start_date = Aws.Util.option_map (Aws.Json.lookup j "StartDate") DateTime.of_json
  ; commitment_duration =
      Aws.Util.option_map (Aws.Json.lookup j "CommitmentDuration") Long.of_json
  ; delivery_preference =
      Aws.Util.option_map
        (Aws.Json.lookup j "DeliveryPreference")
        CapacityReservationDeliveryPreference.of_json
  }
