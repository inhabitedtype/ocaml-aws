open Aws.BaseTypes

type t =
  { instance_count : Integer.t
  ; reserved_instances_offering_id : String.t
  ; purchase_time : DateTime.t option
  ; dry_run : Boolean.t option
  ; limit_price : ReservedInstanceLimitPrice.t option
  }

let make
    ~instance_count
    ~reserved_instances_offering_id
    ?purchase_time
    ?dry_run
    ?limit_price
    () =
  { instance_count; reserved_instances_offering_id; purchase_time; dry_run; limit_price }

let parse xml =
  Some
    { instance_count =
        Aws.Xml.required
          "InstanceCount"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceCount" xml) Integer.parse)
    ; reserved_instances_offering_id =
        Aws.Xml.required
          "ReservedInstancesOfferingId"
          (Aws.Util.option_bind
             (Aws.Xml.member "ReservedInstancesOfferingId" xml)
             String.parse)
    ; purchase_time =
        Aws.Util.option_bind (Aws.Xml.member "PurchaseTime" xml) DateTime.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; limit_price =
        Aws.Util.option_bind
          (Aws.Xml.member "limitPrice" xml)
          ReservedInstanceLimitPrice.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.limit_price (fun f ->
             Aws.Query.Pair ("LimitPrice", ReservedInstanceLimitPrice.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.purchase_time (fun f ->
             Aws.Query.Pair ("PurchaseTime", DateTime.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ReservedInstancesOfferingId"
              , String.to_query v.reserved_instances_offering_id ))
       ; Some (Aws.Query.Pair ("InstanceCount", Integer.to_query v.instance_count))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.limit_price (fun f ->
             "limitPrice", ReservedInstanceLimitPrice.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.purchase_time (fun f -> "PurchaseTime", DateTime.to_json f)
       ; Some
           ("ReservedInstancesOfferingId", String.to_json v.reserved_instances_offering_id)
       ; Some ("InstanceCount", Integer.to_json v.instance_count)
       ])

let of_json j =
  { instance_count =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceCount"))
  ; reserved_instances_offering_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReservedInstancesOfferingId"))
  ; purchase_time =
      Aws.Util.option_map (Aws.Json.lookup j "PurchaseTime") DateTime.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; limit_price =
      Aws.Util.option_map
        (Aws.Json.lookup j "limitPrice")
        ReservedInstanceLimitPrice.of_json
  }
