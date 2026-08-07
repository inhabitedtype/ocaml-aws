open Aws.BaseTypes

type t =
  { currency_code : CurrencyCodeValues.t option
  ; instance_tenancy : Tenancy.t option
  ; offering_class : OfferingClassType.t option
  ; offering_type : OfferingTypeValues.t option
  ; recurring_charges : RecurringChargesList.t
  ; scope : Scope.t option
  ; tags : TagList.t
  ; availability_zone_id : String.t option
  ; reserved_instances_id : String.t option
  ; instance_type : InstanceType.t option
  ; availability_zone : String.t option
  ; start : DateTime.t option
  ; end_ : DateTime.t option
  ; duration : Long.t option
  ; usage_price : Float.t option
  ; fixed_price : Float.t option
  ; instance_count : Integer.t option
  ; product_description : RIProductDescription.t option
  ; state : ReservedInstanceState.t option
  }

let make
    ?currency_code
    ?instance_tenancy
    ?offering_class
    ?offering_type
    ?(recurring_charges = [])
    ?scope
    ?(tags = [])
    ?availability_zone_id
    ?reserved_instances_id
    ?instance_type
    ?availability_zone
    ?start
    ?end_
    ?duration
    ?usage_price
    ?fixed_price
    ?instance_count
    ?product_description
    ?state
    () =
  { currency_code
  ; instance_tenancy
  ; offering_class
  ; offering_type
  ; recurring_charges
  ; scope
  ; tags
  ; availability_zone_id
  ; reserved_instances_id
  ; instance_type
  ; availability_zone
  ; start
  ; end_
  ; duration
  ; usage_price
  ; fixed_price
  ; instance_count
  ; product_description
  ; state
  }

let parse xml =
  Some
    { currency_code =
        Aws.Util.option_bind (Aws.Xml.member "currencyCode" xml) CurrencyCodeValues.parse
    ; instance_tenancy =
        Aws.Util.option_bind (Aws.Xml.member "instanceTenancy" xml) Tenancy.parse
    ; offering_class =
        Aws.Util.option_bind (Aws.Xml.member "offeringClass" xml) OfferingClassType.parse
    ; offering_type =
        Aws.Util.option_bind (Aws.Xml.member "offeringType" xml) OfferingTypeValues.parse
    ; recurring_charges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "recurringCharges" xml)
             RecurringChargesList.parse)
    ; scope = Aws.Util.option_bind (Aws.Xml.member "scope" xml) Scope.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; reserved_instances_id =
        Aws.Util.option_bind (Aws.Xml.member "reservedInstancesId" xml) String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) InstanceType.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; start = Aws.Util.option_bind (Aws.Xml.member "start" xml) DateTime.parse
    ; end_ = Aws.Util.option_bind (Aws.Xml.member "end" xml) DateTime.parse
    ; duration = Aws.Util.option_bind (Aws.Xml.member "duration" xml) Long.parse
    ; usage_price = Aws.Util.option_bind (Aws.Xml.member "usagePrice" xml) Float.parse
    ; fixed_price = Aws.Util.option_bind (Aws.Xml.member "fixedPrice" xml) Float.parse
    ; instance_count =
        Aws.Util.option_bind (Aws.Xml.member "instanceCount" xml) Integer.parse
    ; product_description =
        Aws.Util.option_bind
          (Aws.Xml.member "productDescription" xml)
          RIProductDescription.parse
    ; state =
        Aws.Util.option_bind (Aws.Xml.member "state" xml) ReservedInstanceState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", ReservedInstanceState.to_query f))
       ; Aws.Util.option_map v.product_description (fun f ->
             Aws.Query.Pair ("ProductDescription", RIProductDescription.to_query f))
       ; Aws.Util.option_map v.instance_count (fun f ->
             Aws.Query.Pair ("InstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.fixed_price (fun f ->
             Aws.Query.Pair ("FixedPrice", Float.to_query f))
       ; Aws.Util.option_map v.usage_price (fun f ->
             Aws.Query.Pair ("UsagePrice", Float.to_query f))
       ; Aws.Util.option_map v.duration (fun f ->
             Aws.Query.Pair ("Duration", Long.to_query f))
       ; Aws.Util.option_map v.end_ (fun f -> Aws.Query.Pair ("End", DateTime.to_query f))
       ; Aws.Util.option_map v.start (fun f ->
             Aws.Query.Pair ("Start", DateTime.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", InstanceType.to_query f))
       ; Aws.Util.option_map v.reserved_instances_id (fun f ->
             Aws.Query.Pair ("ReservedInstancesId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.scope (fun f -> Aws.Query.Pair ("Scope", Scope.to_query f))
       ; Some
           (Aws.Query.Pair
              ("RecurringCharges", RecurringChargesList.to_query v.recurring_charges))
       ; Aws.Util.option_map v.offering_type (fun f ->
             Aws.Query.Pair ("OfferingType", OfferingTypeValues.to_query f))
       ; Aws.Util.option_map v.offering_class (fun f ->
             Aws.Query.Pair ("OfferingClass", OfferingClassType.to_query f))
       ; Aws.Util.option_map v.instance_tenancy (fun f ->
             Aws.Query.Pair ("InstanceTenancy", Tenancy.to_query f))
       ; Aws.Util.option_map v.currency_code (fun f ->
             Aws.Query.Pair ("CurrencyCode", CurrencyCodeValues.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state (fun f -> "state", ReservedInstanceState.to_json f)
       ; Aws.Util.option_map v.product_description (fun f ->
             "productDescription", RIProductDescription.to_json f)
       ; Aws.Util.option_map v.instance_count (fun f ->
             "instanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.fixed_price (fun f -> "fixedPrice", Float.to_json f)
       ; Aws.Util.option_map v.usage_price (fun f -> "usagePrice", Float.to_json f)
       ; Aws.Util.option_map v.duration (fun f -> "duration", Long.to_json f)
       ; Aws.Util.option_map v.end_ (fun f -> "end", DateTime.to_json f)
       ; Aws.Util.option_map v.start (fun f -> "start", DateTime.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "instanceType", InstanceType.to_json f)
       ; Aws.Util.option_map v.reserved_instances_id (fun f ->
             "reservedInstancesId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.scope (fun f -> "scope", Scope.to_json f)
       ; Some ("recurringCharges", RecurringChargesList.to_json v.recurring_charges)
       ; Aws.Util.option_map v.offering_type (fun f ->
             "offeringType", OfferingTypeValues.to_json f)
       ; Aws.Util.option_map v.offering_class (fun f ->
             "offeringClass", OfferingClassType.to_json f)
       ; Aws.Util.option_map v.instance_tenancy (fun f ->
             "instanceTenancy", Tenancy.to_json f)
       ; Aws.Util.option_map v.currency_code (fun f ->
             "currencyCode", CurrencyCodeValues.to_json f)
       ])

let of_json j =
  { currency_code =
      Aws.Util.option_map (Aws.Json.lookup j "currencyCode") CurrencyCodeValues.of_json
  ; instance_tenancy =
      Aws.Util.option_map (Aws.Json.lookup j "instanceTenancy") Tenancy.of_json
  ; offering_class =
      Aws.Util.option_map (Aws.Json.lookup j "offeringClass") OfferingClassType.of_json
  ; offering_type =
      Aws.Util.option_map (Aws.Json.lookup j "offeringType") OfferingTypeValues.of_json
  ; recurring_charges =
      RecurringChargesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "recurringCharges"))
  ; scope = Aws.Util.option_map (Aws.Json.lookup j "scope") Scope.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; reserved_instances_id =
      Aws.Util.option_map (Aws.Json.lookup j "reservedInstancesId") String.of_json
  ; instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "instanceType") InstanceType.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; start = Aws.Util.option_map (Aws.Json.lookup j "start") DateTime.of_json
  ; end_ = Aws.Util.option_map (Aws.Json.lookup j "end") DateTime.of_json
  ; duration = Aws.Util.option_map (Aws.Json.lookup j "duration") Long.of_json
  ; usage_price = Aws.Util.option_map (Aws.Json.lookup j "usagePrice") Float.of_json
  ; fixed_price = Aws.Util.option_map (Aws.Json.lookup j "fixedPrice") Float.of_json
  ; instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "instanceCount") Integer.of_json
  ; product_description =
      Aws.Util.option_map
        (Aws.Json.lookup j "productDescription")
        RIProductDescription.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") ReservedInstanceState.of_json
  }
