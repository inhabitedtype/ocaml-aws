open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; event_type : FleetEventType.t option
  ; max_results : Integer.t option
  ; next_token : String.t option
  ; fleet_id : String.t
  ; start_time : DateTime.t
  }

let make ?dry_run ?event_type ?max_results ?next_token ~fleet_id ~start_time () =
  { dry_run; event_type; max_results; next_token; fleet_id; start_time }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; event_type =
        Aws.Util.option_bind (Aws.Xml.member "EventType" xml) FleetEventType.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; fleet_id =
        Aws.Xml.required
          "FleetId"
          (Aws.Util.option_bind (Aws.Xml.member "FleetId" xml) String.parse)
    ; start_time =
        Aws.Xml.required
          "StartTime"
          (Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("StartTime", DateTime.to_query v.start_time))
       ; Some (Aws.Query.Pair ("FleetId", String.to_query v.fleet_id))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.event_type (fun f ->
             Aws.Query.Pair ("EventType", FleetEventType.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("StartTime", DateTime.to_json v.start_time)
       ; Some ("FleetId", String.to_json v.fleet_id)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.event_type (fun f -> "EventType", FleetEventType.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; event_type =
      Aws.Util.option_map (Aws.Json.lookup j "EventType") FleetEventType.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; fleet_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "FleetId"))
  ; start_time = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StartTime"))
  }
