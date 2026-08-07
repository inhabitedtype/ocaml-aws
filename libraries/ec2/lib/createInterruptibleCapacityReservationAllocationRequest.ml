open Aws.BaseTypes

type t =
  { capacity_reservation_id : String.t
  ; instance_count : Integer.t
  ; client_token : String.t option
  ; dry_run : Boolean.t option
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ~capacity_reservation_id
    ~instance_count
    ?client_token
    ?dry_run
    ?(tag_specifications = [])
    () =
  { capacity_reservation_id; instance_count; client_token; dry_run; tag_specifications }

let parse xml =
  Some
    { capacity_reservation_id =
        Aws.Xml.required
          "CapacityReservationId"
          (Aws.Util.option_bind (Aws.Xml.member "CapacityReservationId" xml) String.parse)
    ; instance_count =
        Aws.Xml.required
          "InstanceCount"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceCount" xml) Integer.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
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
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some (Aws.Query.Pair ("InstanceCount", Integer.to_query v.instance_count))
       ; Some
           (Aws.Query.Pair
              ("CapacityReservationId", String.to_query v.capacity_reservation_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("InstanceCount", Integer.to_json v.instance_count)
       ; Some ("CapacityReservationId", String.to_json v.capacity_reservation_id)
       ])

let of_json j =
  { capacity_reservation_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityReservationId"))
  ; instance_count =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceCount"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
