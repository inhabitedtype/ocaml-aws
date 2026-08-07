open Aws.BaseTypes

type t =
  { currency_code : CurrencyCodeValues.t option
  ; instance_tenancy : Tenancy.t option
  ; marketplace : Boolean.t option
  ; offering_class : OfferingClassType.t option
  ; offering_type : OfferingTypeValues.t option
  ; pricing_details : PricingDetailsList.t
  ; recurring_charges : RecurringChargesList.t
  ; scope : Scope.t option
  ; availability_zone_id : String.t option
  ; reserved_instances_offering_id : String.t option
  ; instance_type : InstanceType.t option
  ; availability_zone : String.t option
  ; duration : Long.t option
  ; usage_price : Float.t option
  ; fixed_price : Float.t option
  ; product_description : RIProductDescription.t option
  }

let make
    ?currency_code
    ?instance_tenancy
    ?marketplace
    ?offering_class
    ?offering_type
    ?(pricing_details = [])
    ?(recurring_charges = [])
    ?scope
    ?availability_zone_id
    ?reserved_instances_offering_id
    ?instance_type
    ?availability_zone
    ?duration
    ?usage_price
    ?fixed_price
    ?product_description
    () =
  { currency_code
  ; instance_tenancy
  ; marketplace
  ; offering_class
  ; offering_type
  ; pricing_details
  ; recurring_charges
  ; scope
  ; availability_zone_id
  ; reserved_instances_offering_id
  ; instance_type
  ; availability_zone
  ; duration
  ; usage_price
  ; fixed_price
  ; product_description
  }

let parse xml =
  Some
    { currency_code =
        Aws.Util.option_bind (Aws.Xml.member "currencyCode" xml) CurrencyCodeValues.parse
    ; instance_tenancy =
        Aws.Util.option_bind (Aws.Xml.member "instanceTenancy" xml) Tenancy.parse
    ; marketplace = Aws.Util.option_bind (Aws.Xml.member "marketplace" xml) Boolean.parse
    ; offering_class =
        Aws.Util.option_bind (Aws.Xml.member "offeringClass" xml) OfferingClassType.parse
    ; offering_type =
        Aws.Util.option_bind (Aws.Xml.member "offeringType" xml) OfferingTypeValues.parse
    ; pricing_details =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "pricingDetailsSet" xml)
             PricingDetailsList.parse)
    ; recurring_charges =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "recurringCharges" xml)
             RecurringChargesList.parse)
    ; scope = Aws.Util.option_bind (Aws.Xml.member "scope" xml) Scope.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; reserved_instances_offering_id =
        Aws.Util.option_bind
          (Aws.Xml.member "reservedInstancesOfferingId" xml)
          String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) InstanceType.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; duration = Aws.Util.option_bind (Aws.Xml.member "duration" xml) Long.parse
    ; usage_price = Aws.Util.option_bind (Aws.Xml.member "usagePrice" xml) Float.parse
    ; fixed_price = Aws.Util.option_bind (Aws.Xml.member "fixedPrice" xml) Float.parse
    ; product_description =
        Aws.Util.option_bind
          (Aws.Xml.member "productDescription" xml)
          RIProductDescription.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.product_description (fun f ->
             Aws.Query.Pair ("ProductDescription", RIProductDescription.to_query f))
       ; Aws.Util.option_map v.fixed_price (fun f ->
             Aws.Query.Pair ("FixedPrice", Float.to_query f))
       ; Aws.Util.option_map v.usage_price (fun f ->
             Aws.Query.Pair ("UsagePrice", Float.to_query f))
       ; Aws.Util.option_map v.duration (fun f ->
             Aws.Query.Pair ("Duration", Long.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", InstanceType.to_query f))
       ; Aws.Util.option_map v.reserved_instances_offering_id (fun f ->
             Aws.Query.Pair ("ReservedInstancesOfferingId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.scope (fun f -> Aws.Query.Pair ("Scope", Scope.to_query f))
       ; Some
           (Aws.Query.Pair
              ("RecurringCharges", RecurringChargesList.to_query v.recurring_charges))
       ; Some
           (Aws.Query.Pair
              ("PricingDetailsSet", PricingDetailsList.to_query v.pricing_details))
       ; Aws.Util.option_map v.offering_type (fun f ->
             Aws.Query.Pair ("OfferingType", OfferingTypeValues.to_query f))
       ; Aws.Util.option_map v.offering_class (fun f ->
             Aws.Query.Pair ("OfferingClass", OfferingClassType.to_query f))
       ; Aws.Util.option_map v.marketplace (fun f ->
             Aws.Query.Pair ("Marketplace", Boolean.to_query f))
       ; Aws.Util.option_map v.instance_tenancy (fun f ->
             Aws.Query.Pair ("InstanceTenancy", Tenancy.to_query f))
       ; Aws.Util.option_map v.currency_code (fun f ->
             Aws.Query.Pair ("CurrencyCode", CurrencyCodeValues.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.product_description (fun f ->
             "productDescription", RIProductDescription.to_json f)
       ; Aws.Util.option_map v.fixed_price (fun f -> "fixedPrice", Float.to_json f)
       ; Aws.Util.option_map v.usage_price (fun f -> "usagePrice", Float.to_json f)
       ; Aws.Util.option_map v.duration (fun f -> "duration", Long.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "instanceType", InstanceType.to_json f)
       ; Aws.Util.option_map v.reserved_instances_offering_id (fun f ->
             "reservedInstancesOfferingId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.scope (fun f -> "scope", Scope.to_json f)
       ; Some ("recurringCharges", RecurringChargesList.to_json v.recurring_charges)
       ; Some ("pricingDetailsSet", PricingDetailsList.to_json v.pricing_details)
       ; Aws.Util.option_map v.offering_type (fun f ->
             "offeringType", OfferingTypeValues.to_json f)
       ; Aws.Util.option_map v.offering_class (fun f ->
             "offeringClass", OfferingClassType.to_json f)
       ; Aws.Util.option_map v.marketplace (fun f -> "marketplace", Boolean.to_json f)
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
  ; marketplace = Aws.Util.option_map (Aws.Json.lookup j "marketplace") Boolean.of_json
  ; offering_class =
      Aws.Util.option_map (Aws.Json.lookup j "offeringClass") OfferingClassType.of_json
  ; offering_type =
      Aws.Util.option_map (Aws.Json.lookup j "offeringType") OfferingTypeValues.of_json
  ; pricing_details =
      PricingDetailsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "pricingDetailsSet"))
  ; recurring_charges =
      RecurringChargesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "recurringCharges"))
  ; scope = Aws.Util.option_map (Aws.Json.lookup j "scope") Scope.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; reserved_instances_offering_id =
      Aws.Util.option_map (Aws.Json.lookup j "reservedInstancesOfferingId") String.of_json
  ; instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "instanceType") InstanceType.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; duration = Aws.Util.option_map (Aws.Json.lookup j "duration") Long.of_json
  ; usage_price = Aws.Util.option_map (Aws.Json.lookup j "usagePrice") Float.of_json
  ; fixed_price = Aws.Util.option_map (Aws.Json.lookup j "fixedPrice") Float.of_json
  ; product_description =
      Aws.Util.option_map
        (Aws.Json.lookup j "productDescription")
        RIProductDescription.of_json
  }
