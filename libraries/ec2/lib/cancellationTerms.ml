open Aws.BaseTypes

type t =
  { cancellation_type : ApplyCancellationCharges.t option
  ; reservation_state : String.t option
  ; committed_instance_count : Integer.t option
  ; charge_commitment_duration_hours : Long.t option
  ; charge_end_date : DateTime.t option
  }

let make
    ?cancellation_type
    ?reservation_state
    ?committed_instance_count
    ?charge_commitment_duration_hours
    ?charge_end_date
    () =
  { cancellation_type
  ; reservation_state
  ; committed_instance_count
  ; charge_commitment_duration_hours
  ; charge_end_date
  }

let parse xml =
  Some
    { cancellation_type =
        Aws.Util.option_bind
          (Aws.Xml.member "cancellationType" xml)
          ApplyCancellationCharges.parse
    ; reservation_state =
        Aws.Util.option_bind (Aws.Xml.member "reservationState" xml) String.parse
    ; committed_instance_count =
        Aws.Util.option_bind (Aws.Xml.member "committedInstanceCount" xml) Integer.parse
    ; charge_commitment_duration_hours =
        Aws.Util.option_bind
          (Aws.Xml.member "chargeCommitmentDurationHours" xml)
          Long.parse
    ; charge_end_date =
        Aws.Util.option_bind (Aws.Xml.member "chargeEndDate" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.charge_end_date (fun f ->
             Aws.Query.Pair ("ChargeEndDate", DateTime.to_query f))
       ; Aws.Util.option_map v.charge_commitment_duration_hours (fun f ->
             Aws.Query.Pair ("ChargeCommitmentDurationHours", Long.to_query f))
       ; Aws.Util.option_map v.committed_instance_count (fun f ->
             Aws.Query.Pair ("CommittedInstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.reservation_state (fun f ->
             Aws.Query.Pair ("ReservationState", String.to_query f))
       ; Aws.Util.option_map v.cancellation_type (fun f ->
             Aws.Query.Pair ("CancellationType", ApplyCancellationCharges.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.charge_end_date (fun f ->
             "chargeEndDate", DateTime.to_json f)
       ; Aws.Util.option_map v.charge_commitment_duration_hours (fun f ->
             "chargeCommitmentDurationHours", Long.to_json f)
       ; Aws.Util.option_map v.committed_instance_count (fun f ->
             "committedInstanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.reservation_state (fun f ->
             "reservationState", String.to_json f)
       ; Aws.Util.option_map v.cancellation_type (fun f ->
             "cancellationType", ApplyCancellationCharges.to_json f)
       ])

let of_json j =
  { cancellation_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "cancellationType")
        ApplyCancellationCharges.of_json
  ; reservation_state =
      Aws.Util.option_map (Aws.Json.lookup j "reservationState") String.of_json
  ; committed_instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "committedInstanceCount") Integer.of_json
  ; charge_commitment_duration_hours =
      Aws.Util.option_map (Aws.Json.lookup j "chargeCommitmentDurationHours") Long.of_json
  ; charge_end_date =
      Aws.Util.option_map (Aws.Json.lookup j "chargeEndDate") DateTime.of_json
  }
