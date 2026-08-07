open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; instance_type : String.t option
  ; instance_count : Integer.t option
  ; start_date_range : DateTime.t option
  ; end_date_range : DateTime.t option
  ; capacity_duration_hours : Integer.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  ; ultraserver_type : String.t option
  ; ultraserver_count : Integer.t option
  ; all_availability_zones : Boolean.t option
  }

let make
    ?dry_run
    ?instance_type
    ?instance_count
    ?start_date_range
    ?end_date_range
    ~capacity_duration_hours
    ?next_token
    ?max_results
    ?ultraserver_type
    ?ultraserver_count
    ?all_availability_zones
    () =
  { dry_run
  ; instance_type
  ; instance_count
  ; start_date_range
  ; end_date_range
  ; capacity_duration_hours
  ; next_token
  ; max_results
  ; ultraserver_type
  ; ultraserver_count
  ; all_availability_zones
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "InstanceType" xml) String.parse
    ; instance_count =
        Aws.Util.option_bind (Aws.Xml.member "InstanceCount" xml) Integer.parse
    ; start_date_range =
        Aws.Util.option_bind (Aws.Xml.member "StartDateRange" xml) DateTime.parse
    ; end_date_range =
        Aws.Util.option_bind (Aws.Xml.member "EndDateRange" xml) DateTime.parse
    ; capacity_duration_hours =
        Aws.Xml.required
          "CapacityDurationHours"
          (Aws.Util.option_bind
             (Aws.Xml.member "CapacityDurationHours" xml)
             Integer.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; ultraserver_type =
        Aws.Util.option_bind (Aws.Xml.member "UltraserverType" xml) String.parse
    ; ultraserver_count =
        Aws.Util.option_bind (Aws.Xml.member "UltraserverCount" xml) Integer.parse
    ; all_availability_zones =
        Aws.Util.option_bind (Aws.Xml.member "AllAvailabilityZones" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.all_availability_zones (fun f ->
             Aws.Query.Pair ("AllAvailabilityZones", Boolean.to_query f))
       ; Aws.Util.option_map v.ultraserver_count (fun f ->
             Aws.Query.Pair ("UltraserverCount", Integer.to_query f))
       ; Aws.Util.option_map v.ultraserver_type (fun f ->
             Aws.Query.Pair ("UltraserverType", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("CapacityDurationHours", Integer.to_query v.capacity_duration_hours))
       ; Aws.Util.option_map v.end_date_range (fun f ->
             Aws.Query.Pair ("EndDateRange", DateTime.to_query f))
       ; Aws.Util.option_map v.start_date_range (fun f ->
             Aws.Query.Pair ("StartDateRange", DateTime.to_query f))
       ; Aws.Util.option_map v.instance_count (fun f ->
             Aws.Query.Pair ("InstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.all_availability_zones (fun f ->
             "AllAvailabilityZones", Boolean.to_json f)
       ; Aws.Util.option_map v.ultraserver_count (fun f ->
             "UltraserverCount", Integer.to_json f)
       ; Aws.Util.option_map v.ultraserver_type (fun f ->
             "UltraserverType", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("CapacityDurationHours", Integer.to_json v.capacity_duration_hours)
       ; Aws.Util.option_map v.end_date_range (fun f ->
             "EndDateRange", DateTime.to_json f)
       ; Aws.Util.option_map v.start_date_range (fun f ->
             "StartDateRange", DateTime.to_json f)
       ; Aws.Util.option_map v.instance_count (fun f ->
             "InstanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "InstanceType", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; instance_type = Aws.Util.option_map (Aws.Json.lookup j "InstanceType") String.of_json
  ; instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "InstanceCount") Integer.of_json
  ; start_date_range =
      Aws.Util.option_map (Aws.Json.lookup j "StartDateRange") DateTime.of_json
  ; end_date_range =
      Aws.Util.option_map (Aws.Json.lookup j "EndDateRange") DateTime.of_json
  ; capacity_duration_hours =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityDurationHours"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; ultraserver_type =
      Aws.Util.option_map (Aws.Json.lookup j "UltraserverType") String.of_json
  ; ultraserver_count =
      Aws.Util.option_map (Aws.Json.lookup j "UltraserverCount") Integer.of_json
  ; all_availability_zones =
      Aws.Util.option_map (Aws.Json.lookup j "AllAvailabilityZones") Boolean.of_json
  }
