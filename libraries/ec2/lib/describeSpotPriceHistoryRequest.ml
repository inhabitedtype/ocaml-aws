open Aws.BaseTypes

type t =
  { availability_zone_id : String.t option
  ; dry_run : Boolean.t option
  ; start_time : DateTime.t option
  ; end_time : DateTime.t option
  ; instance_types : InstanceTypeList.t
  ; product_descriptions : ProductDescriptionList.t
  ; filters : FilterList.t
  ; availability_zone : String.t option
  ; max_results : Integer.t option
  ; next_token : String.t option
  }

let make
    ?availability_zone_id
    ?dry_run
    ?start_time
    ?end_time
    ?(instance_types = [])
    ?(product_descriptions = [])
    ?(filters = [])
    ?availability_zone
    ?max_results
    ?next_token
    () =
  { availability_zone_id
  ; dry_run
  ; start_time
  ; end_time
  ; instance_types
  ; product_descriptions
  ; filters
  ; availability_zone
  ; max_results
  ; next_token
  }

let parse xml =
  Some
    { availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZoneId" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "startTime" xml) DateTime.parse
    ; end_time = Aws.Util.option_bind (Aws.Xml.member "endTime" xml) DateTime.parse
    ; instance_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceType" xml)
             InstanceTypeList.parse)
    ; product_descriptions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ProductDescription" xml)
             ProductDescriptionList.parse)
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "maxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Some
           (Aws.Query.Pair
              ( "ProductDescription"
              , ProductDescriptionList.to_query v.product_descriptions ))
       ; Some
           (Aws.Query.Pair ("InstanceType", InstanceTypeList.to_query v.instance_types))
       ; Aws.Util.option_map v.end_time (fun f ->
             Aws.Query.Pair ("EndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "maxResults", Integer.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Some ("ProductDescription", ProductDescriptionList.to_json v.product_descriptions)
       ; Some ("InstanceType", InstanceTypeList.to_json v.instance_types)
       ; Aws.Util.option_map v.end_time (fun f -> "endTime", DateTime.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "startTime", DateTime.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "AvailabilityZoneId", String.to_json f)
       ])

let of_json j =
  { availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZoneId") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "startTime") DateTime.of_json
  ; end_time = Aws.Util.option_map (Aws.Json.lookup j "endTime") DateTime.of_json
  ; instance_types =
      InstanceTypeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceType"))
  ; product_descriptions =
      ProductDescriptionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ProductDescription"))
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "maxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
