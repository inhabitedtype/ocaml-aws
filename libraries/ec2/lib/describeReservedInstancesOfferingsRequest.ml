open Aws.BaseTypes

type t =
  { availability_zone : String.t option
  ; include_marketplace : Boolean.t option
  ; instance_type : InstanceType.t option
  ; max_duration : Long.t option
  ; max_instance_count : Integer.t option
  ; min_duration : Long.t option
  ; offering_class : OfferingClassType.t option
  ; product_description : RIProductDescription.t option
  ; reserved_instances_offering_ids : ReservedInstancesOfferingIdStringList.t
  ; availability_zone_id : String.t option
  ; dry_run : Boolean.t option
  ; filters : FilterList.t
  ; instance_tenancy : Tenancy.t option
  ; offering_type : OfferingTypeValues.t option
  ; next_token : String.t option
  ; max_results : Integer.t option
  }

let make
    ?availability_zone
    ?include_marketplace
    ?instance_type
    ?max_duration
    ?max_instance_count
    ?min_duration
    ?offering_class
    ?product_description
    ?(reserved_instances_offering_ids = [])
    ?availability_zone_id
    ?dry_run
    ?(filters = [])
    ?instance_tenancy
    ?offering_type
    ?next_token
    ?max_results
    () =
  { availability_zone
  ; include_marketplace
  ; instance_type
  ; max_duration
  ; max_instance_count
  ; min_duration
  ; offering_class
  ; product_description
  ; reserved_instances_offering_ids
  ; availability_zone_id
  ; dry_run
  ; filters
  ; instance_tenancy
  ; offering_type
  ; next_token
  ; max_results
  }

let parse xml =
  Some
    { availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse
    ; include_marketplace =
        Aws.Util.option_bind (Aws.Xml.member "IncludeMarketplace" xml) Boolean.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "InstanceType" xml) InstanceType.parse
    ; max_duration = Aws.Util.option_bind (Aws.Xml.member "MaxDuration" xml) Long.parse
    ; max_instance_count =
        Aws.Util.option_bind (Aws.Xml.member "MaxInstanceCount" xml) Integer.parse
    ; min_duration = Aws.Util.option_bind (Aws.Xml.member "MinDuration" xml) Long.parse
    ; offering_class =
        Aws.Util.option_bind (Aws.Xml.member "OfferingClass" xml) OfferingClassType.parse
    ; product_description =
        Aws.Util.option_bind
          (Aws.Xml.member "ProductDescription" xml)
          RIProductDescription.parse
    ; reserved_instances_offering_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReservedInstancesOfferingId" xml)
             ReservedInstancesOfferingIdStringList.parse)
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZoneId" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; instance_tenancy =
        Aws.Util.option_bind (Aws.Xml.member "instanceTenancy" xml) Tenancy.parse
    ; offering_type =
        Aws.Util.option_bind (Aws.Xml.member "offeringType" xml) OfferingTypeValues.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "maxResults" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.offering_type (fun f ->
             Aws.Query.Pair ("OfferingType", OfferingTypeValues.to_query f))
       ; Aws.Util.option_map v.instance_tenancy (fun f ->
             Aws.Query.Pair ("InstanceTenancy", Tenancy.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ReservedInstancesOfferingId"
              , ReservedInstancesOfferingIdStringList.to_query
                  v.reserved_instances_offering_ids ))
       ; Aws.Util.option_map v.product_description (fun f ->
             Aws.Query.Pair ("ProductDescription", RIProductDescription.to_query f))
       ; Aws.Util.option_map v.offering_class (fun f ->
             Aws.Query.Pair ("OfferingClass", OfferingClassType.to_query f))
       ; Aws.Util.option_map v.min_duration (fun f ->
             Aws.Query.Pair ("MinDuration", Long.to_query f))
       ; Aws.Util.option_map v.max_instance_count (fun f ->
             Aws.Query.Pair ("MaxInstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.max_duration (fun f ->
             Aws.Query.Pair ("MaxDuration", Long.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", InstanceType.to_query f))
       ; Aws.Util.option_map v.include_marketplace (fun f ->
             Aws.Query.Pair ("IncludeMarketplace", Boolean.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "maxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Aws.Util.option_map v.offering_type (fun f ->
             "offeringType", OfferingTypeValues.to_json f)
       ; Aws.Util.option_map v.instance_tenancy (fun f ->
             "instanceTenancy", Tenancy.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "AvailabilityZoneId", String.to_json f)
       ; Some
           ( "ReservedInstancesOfferingId"
           , ReservedInstancesOfferingIdStringList.to_json
               v.reserved_instances_offering_ids )
       ; Aws.Util.option_map v.product_description (fun f ->
             "ProductDescription", RIProductDescription.to_json f)
       ; Aws.Util.option_map v.offering_class (fun f ->
             "OfferingClass", OfferingClassType.to_json f)
       ; Aws.Util.option_map v.min_duration (fun f -> "MinDuration", Long.to_json f)
       ; Aws.Util.option_map v.max_instance_count (fun f ->
             "MaxInstanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.max_duration (fun f -> "MaxDuration", Long.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "InstanceType", InstanceType.to_json f)
       ; Aws.Util.option_map v.include_marketplace (fun f ->
             "IncludeMarketplace", Boolean.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "AvailabilityZone", String.to_json f)
       ])

let of_json j =
  { availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZone") String.of_json
  ; include_marketplace =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeMarketplace") Boolean.of_json
  ; instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "InstanceType") InstanceType.of_json
  ; max_duration = Aws.Util.option_map (Aws.Json.lookup j "MaxDuration") Long.of_json
  ; max_instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "MaxInstanceCount") Integer.of_json
  ; min_duration = Aws.Util.option_map (Aws.Json.lookup j "MinDuration") Long.of_json
  ; offering_class =
      Aws.Util.option_map (Aws.Json.lookup j "OfferingClass") OfferingClassType.of_json
  ; product_description =
      Aws.Util.option_map
        (Aws.Json.lookup j "ProductDescription")
        RIProductDescription.of_json
  ; reserved_instances_offering_ids =
      ReservedInstancesOfferingIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReservedInstancesOfferingId"))
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZoneId") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; instance_tenancy =
      Aws.Util.option_map (Aws.Json.lookup j "instanceTenancy") Tenancy.of_json
  ; offering_type =
      Aws.Util.option_map (Aws.Json.lookup j "offeringType") OfferingTypeValues.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "maxResults") Integer.of_json
  }
