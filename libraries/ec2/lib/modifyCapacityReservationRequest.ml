open Aws.BaseTypes

type t =
  { capacity_reservation_id : String.t
  ; instance_count : Integer.t option
  ; end_date : DateTime.t option
  ; end_date_type : EndDateType.t option
  ; accept : Boolean.t option
  ; dry_run : Boolean.t option
  ; additional_info : String.t option
  ; instance_match_criteria : InstanceMatchCriteria.t option
  }

let make
    ~capacity_reservation_id
    ?instance_count
    ?end_date
    ?end_date_type
    ?accept
    ?dry_run
    ?additional_info
    ?instance_match_criteria
    () =
  { capacity_reservation_id
  ; instance_count
  ; end_date
  ; end_date_type
  ; accept
  ; dry_run
  ; additional_info
  ; instance_match_criteria
  }

let parse xml =
  Some
    { capacity_reservation_id =
        Aws.Xml.required
          "CapacityReservationId"
          (Aws.Util.option_bind (Aws.Xml.member "CapacityReservationId" xml) String.parse)
    ; instance_count =
        Aws.Util.option_bind (Aws.Xml.member "InstanceCount" xml) Integer.parse
    ; end_date = Aws.Util.option_bind (Aws.Xml.member "EndDate" xml) DateTime.parse
    ; end_date_type =
        Aws.Util.option_bind (Aws.Xml.member "EndDateType" xml) EndDateType.parse
    ; accept = Aws.Util.option_bind (Aws.Xml.member "Accept" xml) Boolean.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; additional_info =
        Aws.Util.option_bind (Aws.Xml.member "AdditionalInfo" xml) String.parse
    ; instance_match_criteria =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceMatchCriteria" xml)
          InstanceMatchCriteria.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_match_criteria (fun f ->
             Aws.Query.Pair ("InstanceMatchCriteria", InstanceMatchCriteria.to_query f))
       ; Aws.Util.option_map v.additional_info (fun f ->
             Aws.Query.Pair ("AdditionalInfo", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.accept (fun f ->
             Aws.Query.Pair ("Accept", Boolean.to_query f))
       ; Aws.Util.option_map v.end_date_type (fun f ->
             Aws.Query.Pair ("EndDateType", EndDateType.to_query f))
       ; Aws.Util.option_map v.end_date (fun f ->
             Aws.Query.Pair ("EndDate", DateTime.to_query f))
       ; Aws.Util.option_map v.instance_count (fun f ->
             Aws.Query.Pair ("InstanceCount", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("CapacityReservationId", String.to_query v.capacity_reservation_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_match_criteria (fun f ->
             "InstanceMatchCriteria", InstanceMatchCriteria.to_json f)
       ; Aws.Util.option_map v.additional_info (fun f ->
             "AdditionalInfo", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.accept (fun f -> "Accept", Boolean.to_json f)
       ; Aws.Util.option_map v.end_date_type (fun f ->
             "EndDateType", EndDateType.to_json f)
       ; Aws.Util.option_map v.end_date (fun f -> "EndDate", DateTime.to_json f)
       ; Aws.Util.option_map v.instance_count (fun f ->
             "InstanceCount", Integer.to_json f)
       ; Some ("CapacityReservationId", String.to_json v.capacity_reservation_id)
       ])

let of_json j =
  { capacity_reservation_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityReservationId"))
  ; instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "InstanceCount") Integer.of_json
  ; end_date = Aws.Util.option_map (Aws.Json.lookup j "EndDate") DateTime.of_json
  ; end_date_type =
      Aws.Util.option_map (Aws.Json.lookup j "EndDateType") EndDateType.of_json
  ; accept = Aws.Util.option_map (Aws.Json.lookup j "Accept") Boolean.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; additional_info =
      Aws.Util.option_map (Aws.Json.lookup j "AdditionalInfo") String.of_json
  ; instance_match_criteria =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceMatchCriteria")
        InstanceMatchCriteria.of_json
  }
