open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; tag_specifications : TagSpecificationList.t
  ; capacity_block_offering_id : String.t
  ; instance_platform : CapacityReservationInstancePlatform.t
  }

let make
    ?dry_run
    ?(tag_specifications = [])
    ~capacity_block_offering_id
    ~instance_platform
    () =
  { dry_run; tag_specifications; capacity_block_offering_id; instance_platform }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; capacity_block_offering_id =
        Aws.Xml.required
          "CapacityBlockOfferingId"
          (Aws.Util.option_bind
             (Aws.Xml.member "CapacityBlockOfferingId" xml)
             String.parse)
    ; instance_platform =
        Aws.Xml.required
          "InstancePlatform"
          (Aws.Util.option_bind
             (Aws.Xml.member "InstancePlatform" xml)
             CapacityReservationInstancePlatform.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "InstancePlatform"
              , CapacityReservationInstancePlatform.to_query v.instance_platform ))
       ; Some
           (Aws.Query.Pair
              ("CapacityBlockOfferingId", String.to_query v.capacity_block_offering_id))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "InstancePlatform"
           , CapacityReservationInstancePlatform.to_json v.instance_platform )
       ; Some ("CapacityBlockOfferingId", String.to_json v.capacity_block_offering_id)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; capacity_block_offering_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityBlockOfferingId"))
  ; instance_platform =
      CapacityReservationInstancePlatform.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstancePlatform"))
  }
