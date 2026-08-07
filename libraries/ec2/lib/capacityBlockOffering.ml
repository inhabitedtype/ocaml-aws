open Aws.BaseTypes

type t =
  { capacity_block_offering_id : String.t option
  ; instance_type : String.t option
  ; availability_zone : String.t option
  ; instance_count : Integer.t option
  ; start_date : DateTime.t option
  ; end_date : DateTime.t option
  ; capacity_block_duration_hours : Integer.t option
  ; upfront_fee : String.t option
  ; currency_code : String.t option
  ; tenancy : CapacityReservationTenancy.t option
  ; ultraserver_type : String.t option
  ; ultraserver_count : Integer.t option
  ; capacity_block_duration_minutes : Integer.t option
  ; zone_type : String.t option
  }

let make
    ?capacity_block_offering_id
    ?instance_type
    ?availability_zone
    ?instance_count
    ?start_date
    ?end_date
    ?capacity_block_duration_hours
    ?upfront_fee
    ?currency_code
    ?tenancy
    ?ultraserver_type
    ?ultraserver_count
    ?capacity_block_duration_minutes
    ?zone_type
    () =
  { capacity_block_offering_id
  ; instance_type
  ; availability_zone
  ; instance_count
  ; start_date
  ; end_date
  ; capacity_block_duration_hours
  ; upfront_fee
  ; currency_code
  ; tenancy
  ; ultraserver_type
  ; ultraserver_count
  ; capacity_block_duration_minutes
  ; zone_type
  }

let parse xml =
  Some
    { capacity_block_offering_id =
        Aws.Util.option_bind (Aws.Xml.member "capacityBlockOfferingId" xml) String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) String.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; instance_count =
        Aws.Util.option_bind (Aws.Xml.member "instanceCount" xml) Integer.parse
    ; start_date = Aws.Util.option_bind (Aws.Xml.member "startDate" xml) DateTime.parse
    ; end_date = Aws.Util.option_bind (Aws.Xml.member "endDate" xml) DateTime.parse
    ; capacity_block_duration_hours =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityBlockDurationHours" xml)
          Integer.parse
    ; upfront_fee = Aws.Util.option_bind (Aws.Xml.member "upfrontFee" xml) String.parse
    ; currency_code =
        Aws.Util.option_bind (Aws.Xml.member "currencyCode" xml) String.parse
    ; tenancy =
        Aws.Util.option_bind
          (Aws.Xml.member "tenancy" xml)
          CapacityReservationTenancy.parse
    ; ultraserver_type =
        Aws.Util.option_bind (Aws.Xml.member "ultraserverType" xml) String.parse
    ; ultraserver_count =
        Aws.Util.option_bind (Aws.Xml.member "ultraserverCount" xml) Integer.parse
    ; capacity_block_duration_minutes =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityBlockDurationMinutes" xml)
          Integer.parse
    ; zone_type = Aws.Util.option_bind (Aws.Xml.member "zoneType" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.zone_type (fun f ->
             Aws.Query.Pair ("ZoneType", String.to_query f))
       ; Aws.Util.option_map v.capacity_block_duration_minutes (fun f ->
             Aws.Query.Pair ("CapacityBlockDurationMinutes", Integer.to_query f))
       ; Aws.Util.option_map v.ultraserver_count (fun f ->
             Aws.Query.Pair ("UltraserverCount", Integer.to_query f))
       ; Aws.Util.option_map v.ultraserver_type (fun f ->
             Aws.Query.Pair ("UltraserverType", String.to_query f))
       ; Aws.Util.option_map v.tenancy (fun f ->
             Aws.Query.Pair ("Tenancy", CapacityReservationTenancy.to_query f))
       ; Aws.Util.option_map v.currency_code (fun f ->
             Aws.Query.Pair ("CurrencyCode", String.to_query f))
       ; Aws.Util.option_map v.upfront_fee (fun f ->
             Aws.Query.Pair ("UpfrontFee", String.to_query f))
       ; Aws.Util.option_map v.capacity_block_duration_hours (fun f ->
             Aws.Query.Pair ("CapacityBlockDurationHours", Integer.to_query f))
       ; Aws.Util.option_map v.end_date (fun f ->
             Aws.Query.Pair ("EndDate", DateTime.to_query f))
       ; Aws.Util.option_map v.start_date (fun f ->
             Aws.Query.Pair ("StartDate", DateTime.to_query f))
       ; Aws.Util.option_map v.instance_count (fun f ->
             Aws.Query.Pair ("InstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ; Aws.Util.option_map v.capacity_block_offering_id (fun f ->
             Aws.Query.Pair ("CapacityBlockOfferingId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.zone_type (fun f -> "zoneType", String.to_json f)
       ; Aws.Util.option_map v.capacity_block_duration_minutes (fun f ->
             "capacityBlockDurationMinutes", Integer.to_json f)
       ; Aws.Util.option_map v.ultraserver_count (fun f ->
             "ultraserverCount", Integer.to_json f)
       ; Aws.Util.option_map v.ultraserver_type (fun f ->
             "ultraserverType", String.to_json f)
       ; Aws.Util.option_map v.tenancy (fun f ->
             "tenancy", CapacityReservationTenancy.to_json f)
       ; Aws.Util.option_map v.currency_code (fun f -> "currencyCode", String.to_json f)
       ; Aws.Util.option_map v.upfront_fee (fun f -> "upfrontFee", String.to_json f)
       ; Aws.Util.option_map v.capacity_block_duration_hours (fun f ->
             "capacityBlockDurationHours", Integer.to_json f)
       ; Aws.Util.option_map v.end_date (fun f -> "endDate", DateTime.to_json f)
       ; Aws.Util.option_map v.start_date (fun f -> "startDate", DateTime.to_json f)
       ; Aws.Util.option_map v.instance_count (fun f ->
             "instanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "instanceType", String.to_json f)
       ; Aws.Util.option_map v.capacity_block_offering_id (fun f ->
             "capacityBlockOfferingId", String.to_json f)
       ])

let of_json j =
  { capacity_block_offering_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityBlockOfferingId") String.of_json
  ; instance_type = Aws.Util.option_map (Aws.Json.lookup j "instanceType") String.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "instanceCount") Integer.of_json
  ; start_date = Aws.Util.option_map (Aws.Json.lookup j "startDate") DateTime.of_json
  ; end_date = Aws.Util.option_map (Aws.Json.lookup j "endDate") DateTime.of_json
  ; capacity_block_duration_hours =
      Aws.Util.option_map (Aws.Json.lookup j "capacityBlockDurationHours") Integer.of_json
  ; upfront_fee = Aws.Util.option_map (Aws.Json.lookup j "upfrontFee") String.of_json
  ; currency_code = Aws.Util.option_map (Aws.Json.lookup j "currencyCode") String.of_json
  ; tenancy =
      Aws.Util.option_map (Aws.Json.lookup j "tenancy") CapacityReservationTenancy.of_json
  ; ultraserver_type =
      Aws.Util.option_map (Aws.Json.lookup j "ultraserverType") String.of_json
  ; ultraserver_count =
      Aws.Util.option_map (Aws.Json.lookup j "ultraserverCount") Integer.of_json
  ; capacity_block_duration_minutes =
      Aws.Util.option_map
        (Aws.Json.lookup j "capacityBlockDurationMinutes")
        Integer.of_json
  ; zone_type = Aws.Util.option_map (Aws.Json.lookup j "zoneType") String.of_json
  }
