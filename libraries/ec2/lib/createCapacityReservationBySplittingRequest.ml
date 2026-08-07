open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; client_token : String.t option
  ; source_capacity_reservation_id : String.t
  ; instance_count : Integer.t
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ?dry_run
    ?client_token
    ~source_capacity_reservation_id
    ~instance_count
    ?(tag_specifications = [])
    () =
  { dry_run
  ; client_token
  ; source_capacity_reservation_id
  ; instance_count
  ; tag_specifications
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; source_capacity_reservation_id =
        Aws.Xml.required
          "SourceCapacityReservationId"
          (Aws.Util.option_bind
             (Aws.Xml.member "SourceCapacityReservationId" xml)
             String.parse)
    ; instance_count =
        Aws.Xml.required
          "InstanceCount"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceCount" xml) Integer.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Some (Aws.Query.Pair ("InstanceCount", Integer.to_query v.instance_count))
       ; Some
           (Aws.Query.Pair
              ( "SourceCapacityReservationId"
              , String.to_query v.source_capacity_reservation_id ))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some ("InstanceCount", Integer.to_json v.instance_count)
       ; Some
           ("SourceCapacityReservationId", String.to_json v.source_capacity_reservation_id)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; source_capacity_reservation_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceCapacityReservationId"))
  ; instance_count =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceCount"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
