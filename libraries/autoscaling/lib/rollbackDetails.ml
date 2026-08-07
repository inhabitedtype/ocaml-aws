open Aws.BaseTypes

type t =
  { rollback_reason : String.t option
  ; rollback_start_time : DateTime.t option
  ; percentage_complete_on_rollback : Integer.t option
  ; instances_to_update_on_rollback : Integer.t option
  ; progress_details_on_rollback : InstanceRefreshProgressDetails.t option
  }

let make
    ?rollback_reason
    ?rollback_start_time
    ?percentage_complete_on_rollback
    ?instances_to_update_on_rollback
    ?progress_details_on_rollback
    () =
  { rollback_reason
  ; rollback_start_time
  ; percentage_complete_on_rollback
  ; instances_to_update_on_rollback
  ; progress_details_on_rollback
  }

let parse xml =
  Some
    { rollback_reason =
        Aws.Util.option_bind (Aws.Xml.member "RollbackReason" xml) String.parse
    ; rollback_start_time =
        Aws.Util.option_bind (Aws.Xml.member "RollbackStartTime" xml) DateTime.parse
    ; percentage_complete_on_rollback =
        Aws.Util.option_bind
          (Aws.Xml.member "PercentageCompleteOnRollback" xml)
          Integer.parse
    ; instances_to_update_on_rollback =
        Aws.Util.option_bind
          (Aws.Xml.member "InstancesToUpdateOnRollback" xml)
          Integer.parse
    ; progress_details_on_rollback =
        Aws.Util.option_bind
          (Aws.Xml.member "ProgressDetailsOnRollback" xml)
          InstanceRefreshProgressDetails.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.progress_details_on_rollback (fun f ->
             Aws.Query.Pair
               ("ProgressDetailsOnRollback", InstanceRefreshProgressDetails.to_query f))
       ; Aws.Util.option_map v.instances_to_update_on_rollback (fun f ->
             Aws.Query.Pair ("InstancesToUpdateOnRollback", Integer.to_query f))
       ; Aws.Util.option_map v.percentage_complete_on_rollback (fun f ->
             Aws.Query.Pair ("PercentageCompleteOnRollback", Integer.to_query f))
       ; Aws.Util.option_map v.rollback_start_time (fun f ->
             Aws.Query.Pair ("RollbackStartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.rollback_reason (fun f ->
             Aws.Query.Pair ("RollbackReason", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.progress_details_on_rollback (fun f ->
             "ProgressDetailsOnRollback", InstanceRefreshProgressDetails.to_json f)
       ; Aws.Util.option_map v.instances_to_update_on_rollback (fun f ->
             "InstancesToUpdateOnRollback", Integer.to_json f)
       ; Aws.Util.option_map v.percentage_complete_on_rollback (fun f ->
             "PercentageCompleteOnRollback", Integer.to_json f)
       ; Aws.Util.option_map v.rollback_start_time (fun f ->
             "RollbackStartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.rollback_reason (fun f ->
             "RollbackReason", String.to_json f)
       ])

let of_json j =
  { rollback_reason =
      Aws.Util.option_map (Aws.Json.lookup j "RollbackReason") String.of_json
  ; rollback_start_time =
      Aws.Util.option_map (Aws.Json.lookup j "RollbackStartTime") DateTime.of_json
  ; percentage_complete_on_rollback =
      Aws.Util.option_map
        (Aws.Json.lookup j "PercentageCompleteOnRollback")
        Integer.of_json
  ; instances_to_update_on_rollback =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstancesToUpdateOnRollback")
        Integer.of_json
  ; progress_details_on_rollback =
      Aws.Util.option_map
        (Aws.Json.lookup j "ProgressDetailsOnRollback")
        InstanceRefreshProgressDetails.of_json
  }
