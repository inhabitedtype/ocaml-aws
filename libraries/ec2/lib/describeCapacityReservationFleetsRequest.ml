open Aws.BaseTypes

type t =
  { capacity_reservation_fleet_ids : CapacityReservationFleetIdSet.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  ; filters : FilterList.t
  ; dry_run : Boolean.t option
  }

let make
    ?(capacity_reservation_fleet_ids = [])
    ?next_token
    ?max_results
    ?(filters = [])
    ?dry_run
    () =
  { capacity_reservation_fleet_ids; next_token; max_results; filters; dry_run }

let parse xml =
  Some
    { capacity_reservation_fleet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CapacityReservationFleetId" xml)
             CapacityReservationFleetIdSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "CapacityReservationFleetId"
              , CapacityReservationFleetIdSet.to_query v.capacity_reservation_fleet_ids ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some
           ( "CapacityReservationFleetId"
           , CapacityReservationFleetIdSet.to_json v.capacity_reservation_fleet_ids )
       ])

let of_json j =
  { capacity_reservation_fleet_ids =
      CapacityReservationFleetIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityReservationFleetId"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
