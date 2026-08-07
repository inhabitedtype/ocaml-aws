open Aws.BaseTypes

type t =
  { availability_zone : String.t option
  ; create_date : DateTime.t option
  ; hourly_price : String.t option
  ; instance_count : Integer.t option
  ; instance_type : String.t option
  ; network_platform : String.t option
  ; next_slot_start_time : DateTime.t option
  ; platform : String.t option
  ; previous_slot_end_time : DateTime.t option
  ; recurrence : ScheduledInstanceRecurrence.t option
  ; scheduled_instance_id : String.t option
  ; slot_duration_in_hours : Integer.t option
  ; term_end_date : DateTime.t option
  ; term_start_date : DateTime.t option
  ; total_scheduled_instance_hours : Integer.t option
  }

let make
    ?availability_zone
    ?create_date
    ?hourly_price
    ?instance_count
    ?instance_type
    ?network_platform
    ?next_slot_start_time
    ?platform
    ?previous_slot_end_time
    ?recurrence
    ?scheduled_instance_id
    ?slot_duration_in_hours
    ?term_end_date
    ?term_start_date
    ?total_scheduled_instance_hours
    () =
  { availability_zone
  ; create_date
  ; hourly_price
  ; instance_count
  ; instance_type
  ; network_platform
  ; next_slot_start_time
  ; platform
  ; previous_slot_end_time
  ; recurrence
  ; scheduled_instance_id
  ; slot_duration_in_hours
  ; term_end_date
  ; term_start_date
  ; total_scheduled_instance_hours
  }

let parse xml =
  Some
    { availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; create_date = Aws.Util.option_bind (Aws.Xml.member "createDate" xml) DateTime.parse
    ; hourly_price = Aws.Util.option_bind (Aws.Xml.member "hourlyPrice" xml) String.parse
    ; instance_count =
        Aws.Util.option_bind (Aws.Xml.member "instanceCount" xml) Integer.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) String.parse
    ; network_platform =
        Aws.Util.option_bind (Aws.Xml.member "networkPlatform" xml) String.parse
    ; next_slot_start_time =
        Aws.Util.option_bind (Aws.Xml.member "nextSlotStartTime" xml) DateTime.parse
    ; platform = Aws.Util.option_bind (Aws.Xml.member "platform" xml) String.parse
    ; previous_slot_end_time =
        Aws.Util.option_bind (Aws.Xml.member "previousSlotEndTime" xml) DateTime.parse
    ; recurrence =
        Aws.Util.option_bind
          (Aws.Xml.member "recurrence" xml)
          ScheduledInstanceRecurrence.parse
    ; scheduled_instance_id =
        Aws.Util.option_bind (Aws.Xml.member "scheduledInstanceId" xml) String.parse
    ; slot_duration_in_hours =
        Aws.Util.option_bind (Aws.Xml.member "slotDurationInHours" xml) Integer.parse
    ; term_end_date =
        Aws.Util.option_bind (Aws.Xml.member "termEndDate" xml) DateTime.parse
    ; term_start_date =
        Aws.Util.option_bind (Aws.Xml.member "termStartDate" xml) DateTime.parse
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
       ; Aws.Util.option_map v.term_start_date (fun f ->
             Aws.Query.Pair ("TermStartDate", DateTime.to_query f))
       ; Aws.Util.option_map v.term_end_date (fun f ->
             Aws.Query.Pair ("TermEndDate", DateTime.to_query f))
       ; Aws.Util.option_map v.slot_duration_in_hours (fun f ->
             Aws.Query.Pair ("SlotDurationInHours", Integer.to_query f))
       ; Aws.Util.option_map v.scheduled_instance_id (fun f ->
             Aws.Query.Pair ("ScheduledInstanceId", String.to_query f))
       ; Aws.Util.option_map v.recurrence (fun f ->
             Aws.Query.Pair ("Recurrence", ScheduledInstanceRecurrence.to_query f))
       ; Aws.Util.option_map v.previous_slot_end_time (fun f ->
             Aws.Query.Pair ("PreviousSlotEndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.platform (fun f ->
             Aws.Query.Pair ("Platform", String.to_query f))
       ; Aws.Util.option_map v.next_slot_start_time (fun f ->
             Aws.Query.Pair ("NextSlotStartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.network_platform (fun f ->
             Aws.Query.Pair ("NetworkPlatform", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ; Aws.Util.option_map v.instance_count (fun f ->
             Aws.Query.Pair ("InstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.hourly_price (fun f ->
             Aws.Query.Pair ("HourlyPrice", String.to_query f))
       ; Aws.Util.option_map v.create_date (fun f ->
             Aws.Query.Pair ("CreateDate", DateTime.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_scheduled_instance_hours (fun f ->
             "totalScheduledInstanceHours", Integer.to_json f)
       ; Aws.Util.option_map v.term_start_date (fun f ->
             "termStartDate", DateTime.to_json f)
       ; Aws.Util.option_map v.term_end_date (fun f -> "termEndDate", DateTime.to_json f)
       ; Aws.Util.option_map v.slot_duration_in_hours (fun f ->
             "slotDurationInHours", Integer.to_json f)
       ; Aws.Util.option_map v.scheduled_instance_id (fun f ->
             "scheduledInstanceId", String.to_json f)
       ; Aws.Util.option_map v.recurrence (fun f ->
             "recurrence", ScheduledInstanceRecurrence.to_json f)
       ; Aws.Util.option_map v.previous_slot_end_time (fun f ->
             "previousSlotEndTime", DateTime.to_json f)
       ; Aws.Util.option_map v.platform (fun f -> "platform", String.to_json f)
       ; Aws.Util.option_map v.next_slot_start_time (fun f ->
             "nextSlotStartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.network_platform (fun f ->
             "networkPlatform", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "instanceType", String.to_json f)
       ; Aws.Util.option_map v.instance_count (fun f ->
             "instanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.hourly_price (fun f -> "hourlyPrice", String.to_json f)
       ; Aws.Util.option_map v.create_date (fun f -> "createDate", DateTime.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ])

let of_json j =
  { availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; create_date = Aws.Util.option_map (Aws.Json.lookup j "createDate") DateTime.of_json
  ; hourly_price = Aws.Util.option_map (Aws.Json.lookup j "hourlyPrice") String.of_json
  ; instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "instanceCount") Integer.of_json
  ; instance_type = Aws.Util.option_map (Aws.Json.lookup j "instanceType") String.of_json
  ; network_platform =
      Aws.Util.option_map (Aws.Json.lookup j "networkPlatform") String.of_json
  ; next_slot_start_time =
      Aws.Util.option_map (Aws.Json.lookup j "nextSlotStartTime") DateTime.of_json
  ; platform = Aws.Util.option_map (Aws.Json.lookup j "platform") String.of_json
  ; previous_slot_end_time =
      Aws.Util.option_map (Aws.Json.lookup j "previousSlotEndTime") DateTime.of_json
  ; recurrence =
      Aws.Util.option_map
        (Aws.Json.lookup j "recurrence")
        ScheduledInstanceRecurrence.of_json
  ; scheduled_instance_id =
      Aws.Util.option_map (Aws.Json.lookup j "scheduledInstanceId") String.of_json
  ; slot_duration_in_hours =
      Aws.Util.option_map (Aws.Json.lookup j "slotDurationInHours") Integer.of_json
  ; term_end_date = Aws.Util.option_map (Aws.Json.lookup j "termEndDate") DateTime.of_json
  ; term_start_date =
      Aws.Util.option_map (Aws.Json.lookup j "termStartDate") DateTime.of_json
  ; total_scheduled_instance_hours =
      Aws.Util.option_map
        (Aws.Json.lookup j "totalScheduledInstanceHours")
        Integer.of_json
  }
