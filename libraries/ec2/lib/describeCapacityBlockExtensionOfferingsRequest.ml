open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; capacity_block_extension_duration_hours : Integer.t
  ; capacity_reservation_id : String.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  }

let make
    ?dry_run
    ~capacity_block_extension_duration_hours
    ~capacity_reservation_id
    ?next_token
    ?max_results
    () =
  { dry_run
  ; capacity_block_extension_duration_hours
  ; capacity_reservation_id
  ; next_token
  ; max_results
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; capacity_block_extension_duration_hours =
        Aws.Xml.required
          "CapacityBlockExtensionDurationHours"
          (Aws.Util.option_bind
             (Aws.Xml.member "CapacityBlockExtensionDurationHours" xml)
             Integer.parse)
    ; capacity_reservation_id =
        Aws.Xml.required
          "CapacityReservationId"
          (Aws.Util.option_bind (Aws.Xml.member "CapacityReservationId" xml) String.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("CapacityReservationId", String.to_query v.capacity_reservation_id))
       ; Some
           (Aws.Query.Pair
              ( "CapacityBlockExtensionDurationHours"
              , Integer.to_query v.capacity_block_extension_duration_hours ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("CapacityReservationId", String.to_json v.capacity_reservation_id)
       ; Some
           ( "CapacityBlockExtensionDurationHours"
           , Integer.to_json v.capacity_block_extension_duration_hours )
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; capacity_block_extension_duration_hours =
      Integer.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityBlockExtensionDurationHours"))
  ; capacity_reservation_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityReservationId"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  }
