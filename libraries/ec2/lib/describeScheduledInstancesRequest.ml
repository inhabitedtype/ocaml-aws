open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; filters : FilterList.t
  ; max_results : Integer.t option
  ; next_token : String.t option
  ; scheduled_instance_ids : ScheduledInstanceIdRequestSet.t
  ; slot_start_time_range : SlotStartTimeRangeRequest.t option
  }

let make
    ?dry_run
    ?(filters = [])
    ?max_results
    ?next_token
    ?(scheduled_instance_ids = [])
    ?slot_start_time_range
    () =
  { dry_run
  ; filters
  ; max_results
  ; next_token
  ; scheduled_instance_ids
  ; slot_start_time_range
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; scheduled_instance_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ScheduledInstanceId" xml)
             ScheduledInstanceIdRequestSet.parse)
    ; slot_start_time_range =
        Aws.Util.option_bind
          (Aws.Xml.member "SlotStartTimeRange" xml)
          SlotStartTimeRangeRequest.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.slot_start_time_range (fun f ->
             Aws.Query.Pair ("SlotStartTimeRange", SlotStartTimeRangeRequest.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ScheduledInstanceId"
              , ScheduledInstanceIdRequestSet.to_query v.scheduled_instance_ids ))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.slot_start_time_range (fun f ->
             "SlotStartTimeRange", SlotStartTimeRangeRequest.to_json f)
       ; Some
           ( "ScheduledInstanceId"
           , ScheduledInstanceIdRequestSet.to_json v.scheduled_instance_ids )
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; scheduled_instance_ids =
      ScheduledInstanceIdRequestSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ScheduledInstanceId"))
  ; slot_start_time_range =
      Aws.Util.option_map
        (Aws.Json.lookup j "SlotStartTimeRange")
        SlotStartTimeRangeRequest.of_json
  }
