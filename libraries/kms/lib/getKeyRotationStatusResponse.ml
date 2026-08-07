open Aws.BaseTypes

type t =
  { key_rotation_enabled : Boolean.t option
  ; key_id : String.t option
  ; rotation_period_in_days : Integer.t option
  ; next_rotation_date : DateTime.t option
  ; on_demand_rotation_start_date : DateTime.t option
  }

let make
    ?key_rotation_enabled
    ?key_id
    ?rotation_period_in_days
    ?next_rotation_date
    ?on_demand_rotation_start_date
    () =
  { key_rotation_enabled
  ; key_id
  ; rotation_period_in_days
  ; next_rotation_date
  ; on_demand_rotation_start_date
  }

let parse xml =
  Some
    { key_rotation_enabled =
        Aws.Util.option_bind (Aws.Xml.member "KeyRotationEnabled" xml) Boolean.parse
    ; key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
    ; rotation_period_in_days =
        Aws.Util.option_bind (Aws.Xml.member "RotationPeriodInDays" xml) Integer.parse
    ; next_rotation_date =
        Aws.Util.option_bind (Aws.Xml.member "NextRotationDate" xml) DateTime.parse
    ; on_demand_rotation_start_date =
        Aws.Util.option_bind
          (Aws.Xml.member "OnDemandRotationStartDate" xml)
          DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.on_demand_rotation_start_date (fun f ->
             Aws.Query.Pair ("OnDemandRotationStartDate", DateTime.to_query f))
       ; Aws.Util.option_map v.next_rotation_date (fun f ->
             Aws.Query.Pair ("NextRotationDate", DateTime.to_query f))
       ; Aws.Util.option_map v.rotation_period_in_days (fun f ->
             Aws.Query.Pair ("RotationPeriodInDays", Integer.to_query f))
       ; Aws.Util.option_map v.key_id (fun f ->
             Aws.Query.Pair ("KeyId", String.to_query f))
       ; Aws.Util.option_map v.key_rotation_enabled (fun f ->
             Aws.Query.Pair ("KeyRotationEnabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.on_demand_rotation_start_date (fun f ->
             "OnDemandRotationStartDate", DateTime.to_json f)
       ; Aws.Util.option_map v.next_rotation_date (fun f ->
             "NextRotationDate", DateTime.to_json f)
       ; Aws.Util.option_map v.rotation_period_in_days (fun f ->
             "RotationPeriodInDays", Integer.to_json f)
       ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
       ; Aws.Util.option_map v.key_rotation_enabled (fun f ->
             "KeyRotationEnabled", Boolean.to_json f)
       ])

let of_json j =
  { key_rotation_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "KeyRotationEnabled") Boolean.of_json
  ; key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
  ; rotation_period_in_days =
      Aws.Util.option_map (Aws.Json.lookup j "RotationPeriodInDays") Integer.of_json
  ; next_rotation_date =
      Aws.Util.option_map (Aws.Json.lookup j "NextRotationDate") DateTime.of_json
  ; on_demand_rotation_start_date =
      Aws.Util.option_map (Aws.Json.lookup j "OnDemandRotationStartDate") DateTime.of_json
  }
