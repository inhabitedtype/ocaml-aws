open Aws.BaseTypes

type t =
  { offering_class : OfferingClassType.t option
  ; reserved_instances_ids : ReservedInstancesIdStringList.t
  ; dry_run : Boolean.t option
  ; filters : FilterList.t
  ; offering_type : OfferingTypeValues.t option
  }

let make
    ?offering_class
    ?(reserved_instances_ids = [])
    ?dry_run
    ?(filters = [])
    ?offering_type
    () =
  { offering_class; reserved_instances_ids; dry_run; filters; offering_type }

let parse xml =
  Some
    { offering_class =
        Aws.Util.option_bind (Aws.Xml.member "OfferingClass" xml) OfferingClassType.parse
    ; reserved_instances_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReservedInstancesId" xml)
             ReservedInstancesIdStringList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; offering_type =
        Aws.Util.option_bind (Aws.Xml.member "offeringType" xml) OfferingTypeValues.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.offering_type (fun f ->
             Aws.Query.Pair ("OfferingType", OfferingTypeValues.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ReservedInstancesId"
              , ReservedInstancesIdStringList.to_query v.reserved_instances_ids ))
       ; Aws.Util.option_map v.offering_class (fun f ->
             Aws.Query.Pair ("OfferingClass", OfferingClassType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.offering_type (fun f ->
             "offeringType", OfferingTypeValues.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some
           ( "ReservedInstancesId"
           , ReservedInstancesIdStringList.to_json v.reserved_instances_ids )
       ; Aws.Util.option_map v.offering_class (fun f ->
             "OfferingClass", OfferingClassType.to_json f)
       ])

let of_json j =
  { offering_class =
      Aws.Util.option_map (Aws.Json.lookup j "OfferingClass") OfferingClassType.of_json
  ; reserved_instances_ids =
      ReservedInstancesIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReservedInstancesId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; offering_type =
      Aws.Util.option_map (Aws.Json.lookup j "offeringType") OfferingTypeValues.of_json
  }
