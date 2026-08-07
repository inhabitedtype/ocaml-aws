open Aws.BaseTypes

type t =
  { resource_region : String.t option
  ; availability_zone_id : String.t option
  ; account_id : String.t option
  ; account_name : String.t option
  ; instance_family : String.t option
  ; instance_type : String.t option
  ; instance_platform : String.t option
  ; reservation_arn : String.t option
  ; reservation_id : String.t option
  ; reservation_type : ReservationType.t option
  ; reservation_create_timestamp : DateTime.t option
  ; reservation_start_timestamp : DateTime.t option
  ; reservation_end_timestamp : DateTime.t option
  ; reservation_end_date_type : ReservationEndDateType.t option
  ; tenancy : CapacityTenancy.t option
  ; reservation_state : ReservationState.t option
  ; reservation_instance_match_criteria : String.t option
  ; reservation_unused_financial_owner : String.t option
  ; tags : CapacityManagerTagDimensionSet.t
  }

let make
    ?resource_region
    ?availability_zone_id
    ?account_id
    ?account_name
    ?instance_family
    ?instance_type
    ?instance_platform
    ?reservation_arn
    ?reservation_id
    ?reservation_type
    ?reservation_create_timestamp
    ?reservation_start_timestamp
    ?reservation_end_timestamp
    ?reservation_end_date_type
    ?tenancy
    ?reservation_state
    ?reservation_instance_match_criteria
    ?reservation_unused_financial_owner
    ?(tags = [])
    () =
  { resource_region
  ; availability_zone_id
  ; account_id
  ; account_name
  ; instance_family
  ; instance_type
  ; instance_platform
  ; reservation_arn
  ; reservation_id
  ; reservation_type
  ; reservation_create_timestamp
  ; reservation_start_timestamp
  ; reservation_end_timestamp
  ; reservation_end_date_type
  ; tenancy
  ; reservation_state
  ; reservation_instance_match_criteria
  ; reservation_unused_financial_owner
  ; tags
  }

let parse xml =
  Some
    { resource_region =
        Aws.Util.option_bind (Aws.Xml.member "resourceRegion" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; account_id = Aws.Util.option_bind (Aws.Xml.member "accountId" xml) String.parse
    ; account_name = Aws.Util.option_bind (Aws.Xml.member "accountName" xml) String.parse
    ; instance_family =
        Aws.Util.option_bind (Aws.Xml.member "instanceFamily" xml) String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) String.parse
    ; instance_platform =
        Aws.Util.option_bind (Aws.Xml.member "instancePlatform" xml) String.parse
    ; reservation_arn =
        Aws.Util.option_bind (Aws.Xml.member "reservationArn" xml) String.parse
    ; reservation_id =
        Aws.Util.option_bind (Aws.Xml.member "reservationId" xml) String.parse
    ; reservation_type =
        Aws.Util.option_bind (Aws.Xml.member "reservationType" xml) ReservationType.parse
    ; reservation_create_timestamp =
        Aws.Util.option_bind
          (Aws.Xml.member "reservationCreateTimestamp" xml)
          DateTime.parse
    ; reservation_start_timestamp =
        Aws.Util.option_bind
          (Aws.Xml.member "reservationStartTimestamp" xml)
          DateTime.parse
    ; reservation_end_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "reservationEndTimestamp" xml) DateTime.parse
    ; reservation_end_date_type =
        Aws.Util.option_bind
          (Aws.Xml.member "reservationEndDateType" xml)
          ReservationEndDateType.parse
    ; tenancy = Aws.Util.option_bind (Aws.Xml.member "tenancy" xml) CapacityTenancy.parse
    ; reservation_state =
        Aws.Util.option_bind
          (Aws.Xml.member "reservationState" xml)
          ReservationState.parse
    ; reservation_instance_match_criteria =
        Aws.Util.option_bind
          (Aws.Xml.member "reservationInstanceMatchCriteria" xml)
          String.parse
    ; reservation_unused_financial_owner =
        Aws.Util.option_bind
          (Aws.Xml.member "reservationUnusedFinancialOwner" xml)
          String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "tagSet" xml)
             CapacityManagerTagDimensionSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", CapacityManagerTagDimensionSet.to_query v.tags))
       ; Aws.Util.option_map v.reservation_unused_financial_owner (fun f ->
             Aws.Query.Pair ("ReservationUnusedFinancialOwner", String.to_query f))
       ; Aws.Util.option_map v.reservation_instance_match_criteria (fun f ->
             Aws.Query.Pair ("ReservationInstanceMatchCriteria", String.to_query f))
       ; Aws.Util.option_map v.reservation_state (fun f ->
             Aws.Query.Pair ("ReservationState", ReservationState.to_query f))
       ; Aws.Util.option_map v.tenancy (fun f ->
             Aws.Query.Pair ("Tenancy", CapacityTenancy.to_query f))
       ; Aws.Util.option_map v.reservation_end_date_type (fun f ->
             Aws.Query.Pair ("ReservationEndDateType", ReservationEndDateType.to_query f))
       ; Aws.Util.option_map v.reservation_end_timestamp (fun f ->
             Aws.Query.Pair ("ReservationEndTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.reservation_start_timestamp (fun f ->
             Aws.Query.Pair ("ReservationStartTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.reservation_create_timestamp (fun f ->
             Aws.Query.Pair ("ReservationCreateTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.reservation_type (fun f ->
             Aws.Query.Pair ("ReservationType", ReservationType.to_query f))
       ; Aws.Util.option_map v.reservation_id (fun f ->
             Aws.Query.Pair ("ReservationId", String.to_query f))
       ; Aws.Util.option_map v.reservation_arn (fun f ->
             Aws.Query.Pair ("ReservationArn", String.to_query f))
       ; Aws.Util.option_map v.instance_platform (fun f ->
             Aws.Query.Pair ("InstancePlatform", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ; Aws.Util.option_map v.instance_family (fun f ->
             Aws.Query.Pair ("InstanceFamily", String.to_query f))
       ; Aws.Util.option_map v.account_name (fun f ->
             Aws.Query.Pair ("AccountName", String.to_query f))
       ; Aws.Util.option_map v.account_id (fun f ->
             Aws.Query.Pair ("AccountId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.resource_region (fun f ->
             Aws.Query.Pair ("ResourceRegion", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", CapacityManagerTagDimensionSet.to_json v.tags)
       ; Aws.Util.option_map v.reservation_unused_financial_owner (fun f ->
             "reservationUnusedFinancialOwner", String.to_json f)
       ; Aws.Util.option_map v.reservation_instance_match_criteria (fun f ->
             "reservationInstanceMatchCriteria", String.to_json f)
       ; Aws.Util.option_map v.reservation_state (fun f ->
             "reservationState", ReservationState.to_json f)
       ; Aws.Util.option_map v.tenancy (fun f -> "tenancy", CapacityTenancy.to_json f)
       ; Aws.Util.option_map v.reservation_end_date_type (fun f ->
             "reservationEndDateType", ReservationEndDateType.to_json f)
       ; Aws.Util.option_map v.reservation_end_timestamp (fun f ->
             "reservationEndTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.reservation_start_timestamp (fun f ->
             "reservationStartTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.reservation_create_timestamp (fun f ->
             "reservationCreateTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.reservation_type (fun f ->
             "reservationType", ReservationType.to_json f)
       ; Aws.Util.option_map v.reservation_id (fun f -> "reservationId", String.to_json f)
       ; Aws.Util.option_map v.reservation_arn (fun f ->
             "reservationArn", String.to_json f)
       ; Aws.Util.option_map v.instance_platform (fun f ->
             "instancePlatform", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "instanceType", String.to_json f)
       ; Aws.Util.option_map v.instance_family (fun f ->
             "instanceFamily", String.to_json f)
       ; Aws.Util.option_map v.account_name (fun f -> "accountName", String.to_json f)
       ; Aws.Util.option_map v.account_id (fun f -> "accountId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.resource_region (fun f ->
             "resourceRegion", String.to_json f)
       ])

let of_json j =
  { resource_region =
      Aws.Util.option_map (Aws.Json.lookup j "resourceRegion") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; account_id = Aws.Util.option_map (Aws.Json.lookup j "accountId") String.of_json
  ; account_name = Aws.Util.option_map (Aws.Json.lookup j "accountName") String.of_json
  ; instance_family =
      Aws.Util.option_map (Aws.Json.lookup j "instanceFamily") String.of_json
  ; instance_type = Aws.Util.option_map (Aws.Json.lookup j "instanceType") String.of_json
  ; instance_platform =
      Aws.Util.option_map (Aws.Json.lookup j "instancePlatform") String.of_json
  ; reservation_arn =
      Aws.Util.option_map (Aws.Json.lookup j "reservationArn") String.of_json
  ; reservation_id =
      Aws.Util.option_map (Aws.Json.lookup j "reservationId") String.of_json
  ; reservation_type =
      Aws.Util.option_map (Aws.Json.lookup j "reservationType") ReservationType.of_json
  ; reservation_create_timestamp =
      Aws.Util.option_map
        (Aws.Json.lookup j "reservationCreateTimestamp")
        DateTime.of_json
  ; reservation_start_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "reservationStartTimestamp") DateTime.of_json
  ; reservation_end_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "reservationEndTimestamp") DateTime.of_json
  ; reservation_end_date_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "reservationEndDateType")
        ReservationEndDateType.of_json
  ; tenancy = Aws.Util.option_map (Aws.Json.lookup j "tenancy") CapacityTenancy.of_json
  ; reservation_state =
      Aws.Util.option_map (Aws.Json.lookup j "reservationState") ReservationState.of_json
  ; reservation_instance_match_criteria =
      Aws.Util.option_map
        (Aws.Json.lookup j "reservationInstanceMatchCriteria")
        String.of_json
  ; reservation_unused_financial_owner =
      Aws.Util.option_map
        (Aws.Json.lookup j "reservationUnusedFinancialOwner")
        String.of_json
  ; tags =
      CapacityManagerTagDimensionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
