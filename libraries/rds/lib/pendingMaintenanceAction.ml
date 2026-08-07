open Aws.BaseTypes

type t =
  { action : String.t option
  ; auto_applied_after_date : DateTime.t option
  ; forced_apply_date : DateTime.t option
  ; opt_in_status : String.t option
  ; current_apply_date : DateTime.t option
  ; description : String.t option
  }

let make
    ?action
    ?auto_applied_after_date
    ?forced_apply_date
    ?opt_in_status
    ?current_apply_date
    ?description
    () =
  { action
  ; auto_applied_after_date
  ; forced_apply_date
  ; opt_in_status
  ; current_apply_date
  ; description
  }

let parse xml =
  Some
    { action = Aws.Util.option_bind (Aws.Xml.member "Action" xml) String.parse
    ; auto_applied_after_date =
        Aws.Util.option_bind (Aws.Xml.member "AutoAppliedAfterDate" xml) DateTime.parse
    ; forced_apply_date =
        Aws.Util.option_bind (Aws.Xml.member "ForcedApplyDate" xml) DateTime.parse
    ; opt_in_status = Aws.Util.option_bind (Aws.Xml.member "OptInStatus" xml) String.parse
    ; current_apply_date =
        Aws.Util.option_bind (Aws.Xml.member "CurrentApplyDate" xml) DateTime.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.current_apply_date (fun f ->
             Aws.Query.Pair ("CurrentApplyDate", DateTime.to_query f))
       ; Aws.Util.option_map v.opt_in_status (fun f ->
             Aws.Query.Pair ("OptInStatus", String.to_query f))
       ; Aws.Util.option_map v.forced_apply_date (fun f ->
             Aws.Query.Pair ("ForcedApplyDate", DateTime.to_query f))
       ; Aws.Util.option_map v.auto_applied_after_date (fun f ->
             Aws.Query.Pair ("AutoAppliedAfterDate", DateTime.to_query f))
       ; Aws.Util.option_map v.action (fun f ->
             Aws.Query.Pair ("Action", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.current_apply_date (fun f ->
             "CurrentApplyDate", DateTime.to_json f)
       ; Aws.Util.option_map v.opt_in_status (fun f -> "OptInStatus", String.to_json f)
       ; Aws.Util.option_map v.forced_apply_date (fun f ->
             "ForcedApplyDate", DateTime.to_json f)
       ; Aws.Util.option_map v.auto_applied_after_date (fun f ->
             "AutoAppliedAfterDate", DateTime.to_json f)
       ; Aws.Util.option_map v.action (fun f -> "Action", String.to_json f)
       ])

let of_json j =
  { action = Aws.Util.option_map (Aws.Json.lookup j "Action") String.of_json
  ; auto_applied_after_date =
      Aws.Util.option_map (Aws.Json.lookup j "AutoAppliedAfterDate") DateTime.of_json
  ; forced_apply_date =
      Aws.Util.option_map (Aws.Json.lookup j "ForcedApplyDate") DateTime.of_json
  ; opt_in_status = Aws.Util.option_map (Aws.Json.lookup j "OptInStatus") String.of_json
  ; current_apply_date =
      Aws.Util.option_map (Aws.Json.lookup j "CurrentApplyDate") DateTime.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  }
