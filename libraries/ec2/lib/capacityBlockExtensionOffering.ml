open Aws.BaseTypes

type t =
  { capacity_block_extension_offering_id : String.t option
  ; instance_type : String.t option
  ; instance_count : Integer.t option
  ; availability_zone : String.t option
  ; availability_zone_id : String.t option
  ; start_date : DateTime.t option
  ; capacity_block_extension_start_date : DateTime.t option
  ; capacity_block_extension_end_date : DateTime.t option
  ; capacity_block_extension_duration_hours : Integer.t option
  ; upfront_fee : String.t option
  ; currency_code : String.t option
  ; tenancy : CapacityReservationTenancy.t option
  ; zone_type : String.t option
  }

let make
    ?capacity_block_extension_offering_id
    ?instance_type
    ?instance_count
    ?availability_zone
    ?availability_zone_id
    ?start_date
    ?capacity_block_extension_start_date
    ?capacity_block_extension_end_date
    ?capacity_block_extension_duration_hours
    ?upfront_fee
    ?currency_code
    ?tenancy
    ?zone_type
    () =
  { capacity_block_extension_offering_id
  ; instance_type
  ; instance_count
  ; availability_zone
  ; availability_zone_id
  ; start_date
  ; capacity_block_extension_start_date
  ; capacity_block_extension_end_date
  ; capacity_block_extension_duration_hours
  ; upfront_fee
  ; currency_code
  ; tenancy
  ; zone_type
  }

let parse xml =
  Some
    { capacity_block_extension_offering_id =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityBlockExtensionOfferingId" xml)
          String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) String.parse
    ; instance_count =
        Aws.Util.option_bind (Aws.Xml.member "instanceCount" xml) Integer.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; start_date = Aws.Util.option_bind (Aws.Xml.member "startDate" xml) DateTime.parse
    ; capacity_block_extension_start_date =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityBlockExtensionStartDate" xml)
          DateTime.parse
    ; capacity_block_extension_end_date =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityBlockExtensionEndDate" xml)
          DateTime.parse
    ; capacity_block_extension_duration_hours =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityBlockExtensionDurationHours" xml)
          Integer.parse
    ; upfront_fee = Aws.Util.option_bind (Aws.Xml.member "upfrontFee" xml) String.parse
    ; currency_code =
        Aws.Util.option_bind (Aws.Xml.member "currencyCode" xml) String.parse
    ; tenancy =
        Aws.Util.option_bind
          (Aws.Xml.member "tenancy" xml)
          CapacityReservationTenancy.parse
    ; zone_type = Aws.Util.option_bind (Aws.Xml.member "zoneType" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.zone_type (fun f ->
             Aws.Query.Pair ("ZoneType", String.to_query f))
       ; Aws.Util.option_map v.tenancy (fun f ->
             Aws.Query.Pair ("Tenancy", CapacityReservationTenancy.to_query f))
       ; Aws.Util.option_map v.currency_code (fun f ->
             Aws.Query.Pair ("CurrencyCode", String.to_query f))
       ; Aws.Util.option_map v.upfront_fee (fun f ->
             Aws.Query.Pair ("UpfrontFee", String.to_query f))
       ; Aws.Util.option_map v.capacity_block_extension_duration_hours (fun f ->
             Aws.Query.Pair ("CapacityBlockExtensionDurationHours", Integer.to_query f))
       ; Aws.Util.option_map v.capacity_block_extension_end_date (fun f ->
             Aws.Query.Pair ("CapacityBlockExtensionEndDate", DateTime.to_query f))
       ; Aws.Util.option_map v.capacity_block_extension_start_date (fun f ->
             Aws.Query.Pair ("CapacityBlockExtensionStartDate", DateTime.to_query f))
       ; Aws.Util.option_map v.start_date (fun f ->
             Aws.Query.Pair ("StartDate", DateTime.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.instance_count (fun f ->
             Aws.Query.Pair ("InstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ; Aws.Util.option_map v.capacity_block_extension_offering_id (fun f ->
             Aws.Query.Pair ("CapacityBlockExtensionOfferingId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.zone_type (fun f -> "zoneType", String.to_json f)
       ; Aws.Util.option_map v.tenancy (fun f ->
             "tenancy", CapacityReservationTenancy.to_json f)
       ; Aws.Util.option_map v.currency_code (fun f -> "currencyCode", String.to_json f)
       ; Aws.Util.option_map v.upfront_fee (fun f -> "upfrontFee", String.to_json f)
       ; Aws.Util.option_map v.capacity_block_extension_duration_hours (fun f ->
             "capacityBlockExtensionDurationHours", Integer.to_json f)
       ; Aws.Util.option_map v.capacity_block_extension_end_date (fun f ->
             "capacityBlockExtensionEndDate", DateTime.to_json f)
       ; Aws.Util.option_map v.capacity_block_extension_start_date (fun f ->
             "capacityBlockExtensionStartDate", DateTime.to_json f)
       ; Aws.Util.option_map v.start_date (fun f -> "startDate", DateTime.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.instance_count (fun f ->
             "instanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "instanceType", String.to_json f)
       ; Aws.Util.option_map v.capacity_block_extension_offering_id (fun f ->
             "capacityBlockExtensionOfferingId", String.to_json f)
       ])

let of_json j =
  { capacity_block_extension_offering_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "capacityBlockExtensionOfferingId")
        String.of_json
  ; instance_type = Aws.Util.option_map (Aws.Json.lookup j "instanceType") String.of_json
  ; instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "instanceCount") Integer.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; start_date = Aws.Util.option_map (Aws.Json.lookup j "startDate") DateTime.of_json
  ; capacity_block_extension_start_date =
      Aws.Util.option_map
        (Aws.Json.lookup j "capacityBlockExtensionStartDate")
        DateTime.of_json
  ; capacity_block_extension_end_date =
      Aws.Util.option_map
        (Aws.Json.lookup j "capacityBlockExtensionEndDate")
        DateTime.of_json
  ; capacity_block_extension_duration_hours =
      Aws.Util.option_map
        (Aws.Json.lookup j "capacityBlockExtensionDurationHours")
        Integer.of_json
  ; upfront_fee = Aws.Util.option_map (Aws.Json.lookup j "upfrontFee") String.of_json
  ; currency_code = Aws.Util.option_map (Aws.Json.lookup j "currencyCode") String.of_json
  ; tenancy =
      Aws.Util.option_map (Aws.Json.lookup j "tenancy") CapacityReservationTenancy.of_json
  ; zone_type = Aws.Util.option_map (Aws.Json.lookup j "zoneType") String.of_json
  }
