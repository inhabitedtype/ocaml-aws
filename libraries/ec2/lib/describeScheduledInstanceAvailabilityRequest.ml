open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; filters : FilterList.t
  ; first_slot_start_time_range : SlotDateTimeRangeRequest.t
  ; max_results : Integer.t option
  ; max_slot_duration_in_hours : Integer.t option
  ; min_slot_duration_in_hours : Integer.t option
  ; next_token : String.t option
  ; recurrence : ScheduledInstanceRecurrenceRequest.t
  }

let make
    ?dry_run
    ?(filters = [])
    ~first_slot_start_time_range
    ?max_results
    ?max_slot_duration_in_hours
    ?min_slot_duration_in_hours
    ?next_token
    ~recurrence
    () =
  { dry_run
  ; filters
  ; first_slot_start_time_range
  ; max_results
  ; max_slot_duration_in_hours
  ; min_slot_duration_in_hours
  ; next_token
  ; recurrence
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; first_slot_start_time_range =
        Aws.Xml.required
          "FirstSlotStartTimeRange"
          (Aws.Util.option_bind
             (Aws.Xml.member "FirstSlotStartTimeRange" xml)
             SlotDateTimeRangeRequest.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; max_slot_duration_in_hours =
        Aws.Util.option_bind (Aws.Xml.member "MaxSlotDurationInHours" xml) Integer.parse
    ; min_slot_duration_in_hours =
        Aws.Util.option_bind (Aws.Xml.member "MinSlotDurationInHours" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; recurrence =
        Aws.Xml.required
          "Recurrence"
          (Aws.Util.option_bind
             (Aws.Xml.member "Recurrence" xml)
             ScheduledInstanceRecurrenceRequest.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("Recurrence", ScheduledInstanceRecurrenceRequest.to_query v.recurrence))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.min_slot_duration_in_hours (fun f ->
             Aws.Query.Pair ("MinSlotDurationInHours", Integer.to_query f))
       ; Aws.Util.option_map v.max_slot_duration_in_hours (fun f ->
             Aws.Query.Pair ("MaxSlotDurationInHours", Integer.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "FirstSlotStartTimeRange"
              , SlotDateTimeRangeRequest.to_query v.first_slot_start_time_range ))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Recurrence", ScheduledInstanceRecurrenceRequest.to_json v.recurrence)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.min_slot_duration_in_hours (fun f ->
             "MinSlotDurationInHours", Integer.to_json f)
       ; Aws.Util.option_map v.max_slot_duration_in_hours (fun f ->
             "MaxSlotDurationInHours", Integer.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some
           ( "FirstSlotStartTimeRange"
           , SlotDateTimeRangeRequest.to_json v.first_slot_start_time_range )
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; first_slot_start_time_range =
      SlotDateTimeRangeRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "FirstSlotStartTimeRange"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; max_slot_duration_in_hours =
      Aws.Util.option_map (Aws.Json.lookup j "MaxSlotDurationInHours") Integer.of_json
  ; min_slot_duration_in_hours =
      Aws.Util.option_map (Aws.Json.lookup j "MinSlotDurationInHours") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; recurrence =
      ScheduledInstanceRecurrenceRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Recurrence"))
  }
