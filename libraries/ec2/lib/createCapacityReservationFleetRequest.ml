open Aws.BaseTypes

type t =
  { allocation_strategy : String.t option
  ; client_token : String.t option
  ; instance_type_specifications : ReservationFleetInstanceSpecificationList.t
  ; tenancy : FleetCapacityReservationTenancy.t option
  ; total_target_capacity : Integer.t
  ; end_date : DateTime.t option
  ; instance_match_criteria : FleetInstanceMatchCriteria.t option
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  }

let make
    ?allocation_strategy
    ?client_token
    ?(instance_type_specifications = [])
    ?tenancy
    ~total_target_capacity
    ?end_date
    ?instance_match_criteria
    ?(tag_specifications = [])
    ?dry_run
    () =
  { allocation_strategy
  ; client_token
  ; instance_type_specifications
  ; tenancy
  ; total_target_capacity
  ; end_date
  ; instance_match_criteria
  ; tag_specifications
  ; dry_run
  }

let parse xml =
  Some
    { allocation_strategy =
        Aws.Util.option_bind (Aws.Xml.member "AllocationStrategy" xml) String.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; instance_type_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceTypeSpecification" xml)
             ReservationFleetInstanceSpecificationList.parse)
    ; tenancy =
        Aws.Util.option_bind
          (Aws.Xml.member "Tenancy" xml)
          FleetCapacityReservationTenancy.parse
    ; total_target_capacity =
        Aws.Xml.required
          "TotalTargetCapacity"
          (Aws.Util.option_bind (Aws.Xml.member "TotalTargetCapacity" xml) Integer.parse)
    ; end_date = Aws.Util.option_bind (Aws.Xml.member "EndDate" xml) DateTime.parse
    ; instance_match_criteria =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceMatchCriteria" xml)
          FleetInstanceMatchCriteria.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.instance_match_criteria (fun f ->
             Aws.Query.Pair
               ("InstanceMatchCriteria", FleetInstanceMatchCriteria.to_query f))
       ; Aws.Util.option_map v.end_date (fun f ->
             Aws.Query.Pair ("EndDate", DateTime.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TotalTargetCapacity", Integer.to_query v.total_target_capacity))
       ; Aws.Util.option_map v.tenancy (fun f ->
             Aws.Query.Pair ("Tenancy", FleetCapacityReservationTenancy.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "InstanceTypeSpecification"
              , ReservationFleetInstanceSpecificationList.to_query
                  v.instance_type_specifications ))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.allocation_strategy (fun f ->
             Aws.Query.Pair ("AllocationStrategy", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.instance_match_criteria (fun f ->
             "InstanceMatchCriteria", FleetInstanceMatchCriteria.to_json f)
       ; Aws.Util.option_map v.end_date (fun f -> "EndDate", DateTime.to_json f)
       ; Some ("TotalTargetCapacity", Integer.to_json v.total_target_capacity)
       ; Aws.Util.option_map v.tenancy (fun f ->
             "Tenancy", FleetCapacityReservationTenancy.to_json f)
       ; Some
           ( "InstanceTypeSpecification"
           , ReservationFleetInstanceSpecificationList.to_json
               v.instance_type_specifications )
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.allocation_strategy (fun f ->
             "AllocationStrategy", String.to_json f)
       ])

let of_json j =
  { allocation_strategy =
      Aws.Util.option_map (Aws.Json.lookup j "AllocationStrategy") String.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; instance_type_specifications =
      ReservationFleetInstanceSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceTypeSpecification"))
  ; tenancy =
      Aws.Util.option_map
        (Aws.Json.lookup j "Tenancy")
        FleetCapacityReservationTenancy.of_json
  ; total_target_capacity =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TotalTargetCapacity"))
  ; end_date = Aws.Util.option_map (Aws.Json.lookup j "EndDate") DateTime.of_json
  ; instance_match_criteria =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceMatchCriteria")
        FleetInstanceMatchCriteria.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
