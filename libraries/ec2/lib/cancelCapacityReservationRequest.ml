open Aws.BaseTypes

type t =
  { capacity_reservation_id : String.t
  ; dry_run : Boolean.t option
  ; apply_cancellation_charges : ApplyCancellationCharges.t option
  ; quote_id : String.t option
  }

let make ~capacity_reservation_id ?dry_run ?apply_cancellation_charges ?quote_id () =
  { capacity_reservation_id; dry_run; apply_cancellation_charges; quote_id }

let parse xml =
  Some
    { capacity_reservation_id =
        Aws.Xml.required
          "CapacityReservationId"
          (Aws.Util.option_bind (Aws.Xml.member "CapacityReservationId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; apply_cancellation_charges =
        Aws.Util.option_bind
          (Aws.Xml.member "ApplyCancellationCharges" xml)
          ApplyCancellationCharges.parse
    ; quote_id = Aws.Util.option_bind (Aws.Xml.member "QuoteId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.quote_id (fun f ->
             Aws.Query.Pair ("QuoteId", String.to_query f))
       ; Aws.Util.option_map v.apply_cancellation_charges (fun f ->
             Aws.Query.Pair
               ("ApplyCancellationCharges", ApplyCancellationCharges.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("CapacityReservationId", String.to_query v.capacity_reservation_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.quote_id (fun f -> "QuoteId", String.to_json f)
       ; Aws.Util.option_map v.apply_cancellation_charges (fun f ->
             "ApplyCancellationCharges", ApplyCancellationCharges.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("CapacityReservationId", String.to_json v.capacity_reservation_id)
       ])

let of_json j =
  { capacity_reservation_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityReservationId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; apply_cancellation_charges =
      Aws.Util.option_map
        (Aws.Json.lookup j "ApplyCancellationCharges")
        ApplyCancellationCharges.of_json
  ; quote_id = Aws.Util.option_map (Aws.Json.lookup j "QuoteId") String.of_json
  }
