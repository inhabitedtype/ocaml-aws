open Aws.BaseTypes

type t =
  { availability_zone : String.t option
  ; available_instance_count : Integer.t option
  ; first_slot_start_time : DateTime.t option
  ; hourly_price : String.t option
  ; instance_type : String.t option
  ; max_term_duration_in_days : Integer.t option
  ; min_term_duration_in_days : Integer.t option
  ; network_platform : String.t option
  ; platform : String.t option
  ; purchase_token : String.t option
  ; recurrence : ScheduledInstanceRecurrence.t option
  ; slot_duration_in_hours : Integer.t option
  ; total_scheduled_instance_hours : Integer.t option
  }

let make
    ?availability_zone
    ?available_instance_count
    ?first_slot_start_time
    ?hourly_price
    ?instance_type
    ?max_term_duration_in_days
    ?min_term_duration_in_days
    ?network_platform
    ?platform
    ?purchase_token
    ?recurrence
    ?slot_duration_in_hours
    ?total_scheduled_instance_hours
    () =
  { availability_zone
  ; available_instance_count
  ; first_slot_start_time
  ; hourly_price
  ; instance_type
  ; max_term_duration_in_days
  ; min_term_duration_in_days
  ; network_platform
  ; platform
  ; purchase_token
  ; recurrence
  ; slot_duration_in_hours
  ; total_scheduled_instance_hours
  }

let parse xml =
  Some
    { availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; available_instance_count =
        Aws.Util.option_bind (Aws.Xml.member "availableInstanceCount" xml) Integer.parse
    ; first_slot_start_time =
        Aws.Util.option_bind (Aws.Xml.member "firstSlotStartTime" xml) DateTime.parse
    ; hourly_price = Aws.Util.option_bind (Aws.Xml.member "hourlyPrice" xml) String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) String.parse
    ; max_term_duration_in_days =
        Aws.Util.option_bind (Aws.Xml.member "maxTermDurationInDays" xml) Integer.parse
    ; min_term_duration_in_days =
        Aws.Util.option_bind (Aws.Xml.member "minTermDurationInDays" xml) Integer.parse
    ; network_platform =
        Aws.Util.option_bind (Aws.Xml.member "networkPlatform" xml) String.parse
    ; platform = Aws.Util.option_bind (Aws.Xml.member "platform" xml) String.parse
    ; purchase_token =
        Aws.Util.option_bind (Aws.Xml.member "purchaseToken" xml) String.parse
    ; recurrence =
        Aws.Util.option_bind
          (Aws.Xml.member "recurrence" xml)
          ScheduledInstanceRecurrence.parse
    ; slot_duration_in_hours =
        Aws.Util.option_bind (Aws.Xml.member "slotDurationInHours" xml) Integer.parse
    ; total_scheduled_instance_hours =
        Aws.Util.option_bind
          (Aws.Xml.member "totalScheduledInstanceHours" xml)
          Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_scheduled_instance_hours (fun f ->
             Aws.Query.Pair ("TotalScheduledInstanceHours", Integer.to_query f))
       ; Aws.Util.option_map v.slot_duration_in_hours (fun f ->
             Aws.Query.Pair ("SlotDurationInHours", Integer.to_query f))
       ; Aws.Util.option_map v.recurrence (fun f ->
             Aws.Query.Pair ("Recurrence", ScheduledInstanceRecurrence.to_query f))
       ; Aws.Util.option_map v.purchase_token (fun f ->
             Aws.Query.Pair ("PurchaseToken", String.to_query f))
       ; Aws.Util.option_map v.platform (fun f ->
             Aws.Query.Pair ("Platform", String.to_query f))
       ; Aws.Util.option_map v.network_platform (fun f ->
             Aws.Query.Pair ("NetworkPlatform", String.to_query f))
       ; Aws.Util.option_map v.min_term_duration_in_days (fun f ->
             Aws.Query.Pair ("MinTermDurationInDays", Integer.to_query f))
       ; Aws.Util.option_map v.max_term_duration_in_days (fun f ->
             Aws.Query.Pair ("MaxTermDurationInDays", Integer.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ; Aws.Util.option_map v.hourly_price (fun f ->
             Aws.Query.Pair ("HourlyPrice", String.to_query f))
       ; Aws.Util.option_map v.first_slot_start_time (fun f ->
             Aws.Query.Pair ("FirstSlotStartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.available_instance_count (fun f ->
             Aws.Query.Pair ("AvailableInstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_scheduled_instance_hours (fun f ->
             "totalScheduledInstanceHours", Integer.to_json f)
       ; Aws.Util.option_map v.slot_duration_in_hours (fun f ->
             "slotDurationInHours", Integer.to_json f)
       ; Aws.Util.option_map v.recurrence (fun f ->
             "recurrence", ScheduledInstanceRecurrence.to_json f)
       ; Aws.Util.option_map v.purchase_token (fun f -> "purchaseToken", String.to_json f)
       ; Aws.Util.option_map v.platform (fun f -> "platform", String.to_json f)
       ; Aws.Util.option_map v.network_platform (fun f ->
             "networkPlatform", String.to_json f)
       ; Aws.Util.option_map v.min_term_duration_in_days (fun f ->
             "minTermDurationInDays", Integer.to_json f)
       ; Aws.Util.option_map v.max_term_duration_in_days (fun f ->
             "maxTermDurationInDays", Integer.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "instanceType", String.to_json f)
       ; Aws.Util.option_map v.hourly_price (fun f -> "hourlyPrice", String.to_json f)
       ; Aws.Util.option_map v.first_slot_start_time (fun f ->
             "firstSlotStartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.available_instance_count (fun f ->
             "availableInstanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ])

let of_json j =
  { availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; available_instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "availableInstanceCount") Integer.of_json
  ; first_slot_start_time =
      Aws.Util.option_map (Aws.Json.lookup j "firstSlotStartTime") DateTime.of_json
  ; hourly_price = Aws.Util.option_map (Aws.Json.lookup j "hourlyPrice") String.of_json
  ; instance_type = Aws.Util.option_map (Aws.Json.lookup j "instanceType") String.of_json
  ; max_term_duration_in_days =
      Aws.Util.option_map (Aws.Json.lookup j "maxTermDurationInDays") Integer.of_json
  ; min_term_duration_in_days =
      Aws.Util.option_map (Aws.Json.lookup j "minTermDurationInDays") Integer.of_json
  ; network_platform =
      Aws.Util.option_map (Aws.Json.lookup j "networkPlatform") String.of_json
  ; platform = Aws.Util.option_map (Aws.Json.lookup j "platform") String.of_json
  ; purchase_token =
      Aws.Util.option_map (Aws.Json.lookup j "purchaseToken") String.of_json
  ; recurrence =
      Aws.Util.option_map
        (Aws.Json.lookup j "recurrence")
        ScheduledInstanceRecurrence.of_json
  ; slot_duration_in_hours =
      Aws.Util.option_map (Aws.Json.lookup j "slotDurationInHours") Integer.of_json
  ; total_scheduled_instance_hours =
      Aws.Util.option_map
        (Aws.Json.lookup j "totalScheduledInstanceHours")
        Integer.of_json
  }
