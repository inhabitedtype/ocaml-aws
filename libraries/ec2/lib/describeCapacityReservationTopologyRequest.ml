open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; next_token : String.t option
  ; max_results : Integer.t option
  ; capacity_reservation_ids : CapacityReservationIdSet.t
  ; filters : FilterList.t
  }

let make
    ?dry_run
    ?next_token
    ?max_results
    ?(capacity_reservation_ids = [])
    ?(filters = [])
    () =
  { dry_run; next_token; max_results; capacity_reservation_ids; filters }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; capacity_reservation_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CapacityReservationId" xml)
             CapacityReservationIdSet.parse)
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Some
           (Aws.Query.Pair
              ( "CapacityReservationId"
              , CapacityReservationIdSet.to_query v.capacity_reservation_ids ))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filter", FilterList.to_json v.filters)
       ; Some
           ( "CapacityReservationId"
           , CapacityReservationIdSet.to_json v.capacity_reservation_ids )
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; capacity_reservation_ids =
      CapacityReservationIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityReservationId"))
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  }
