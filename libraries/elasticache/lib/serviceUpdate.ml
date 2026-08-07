open Aws.BaseTypes

type t =
  { service_update_name : String.t option
  ; service_update_release_date : DateTime.t option
  ; service_update_end_date : DateTime.t option
  ; service_update_severity : ServiceUpdateSeverity.t option
  ; service_update_recommended_apply_by_date : DateTime.t option
  ; service_update_status : ServiceUpdateStatus.t option
  ; service_update_description : String.t option
  ; service_update_type : ServiceUpdateType.t option
  ; engine : String.t option
  ; engine_version : String.t option
  ; auto_update_after_recommended_apply_by_date : Boolean.t option
  ; estimated_update_time : String.t option
  }

let make
    ?service_update_name
    ?service_update_release_date
    ?service_update_end_date
    ?service_update_severity
    ?service_update_recommended_apply_by_date
    ?service_update_status
    ?service_update_description
    ?service_update_type
    ?engine
    ?engine_version
    ?auto_update_after_recommended_apply_by_date
    ?estimated_update_time
    () =
  { service_update_name
  ; service_update_release_date
  ; service_update_end_date
  ; service_update_severity
  ; service_update_recommended_apply_by_date
  ; service_update_status
  ; service_update_description
  ; service_update_type
  ; engine
  ; engine_version
  ; auto_update_after_recommended_apply_by_date
  ; estimated_update_time
  }

let parse xml =
  Some
    { service_update_name =
        Aws.Util.option_bind (Aws.Xml.member "ServiceUpdateName" xml) String.parse
    ; service_update_release_date =
        Aws.Util.option_bind
          (Aws.Xml.member "ServiceUpdateReleaseDate" xml)
          DateTime.parse
    ; service_update_end_date =
        Aws.Util.option_bind (Aws.Xml.member "ServiceUpdateEndDate" xml) DateTime.parse
    ; service_update_severity =
        Aws.Util.option_bind
          (Aws.Xml.member "ServiceUpdateSeverity" xml)
          ServiceUpdateSeverity.parse
    ; service_update_recommended_apply_by_date =
        Aws.Util.option_bind
          (Aws.Xml.member "ServiceUpdateRecommendedApplyByDate" xml)
          DateTime.parse
    ; service_update_status =
        Aws.Util.option_bind
          (Aws.Xml.member "ServiceUpdateStatus" xml)
          ServiceUpdateStatus.parse
    ; service_update_description =
        Aws.Util.option_bind (Aws.Xml.member "ServiceUpdateDescription" xml) String.parse
    ; service_update_type =
        Aws.Util.option_bind
          (Aws.Xml.member "ServiceUpdateType" xml)
          ServiceUpdateType.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; auto_update_after_recommended_apply_by_date =
        Aws.Util.option_bind
          (Aws.Xml.member "AutoUpdateAfterRecommendedApplyByDate" xml)
          Boolean.parse
    ; estimated_update_time =
        Aws.Util.option_bind (Aws.Xml.member "EstimatedUpdateTime" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.estimated_update_time (fun f ->
             Aws.Query.Pair ("EstimatedUpdateTime", String.to_query f))
       ; Aws.Util.option_map v.auto_update_after_recommended_apply_by_date (fun f ->
             Aws.Query.Pair ("AutoUpdateAfterRecommendedApplyByDate", Boolean.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.service_update_type (fun f ->
             Aws.Query.Pair ("ServiceUpdateType", ServiceUpdateType.to_query f))
       ; Aws.Util.option_map v.service_update_description (fun f ->
             Aws.Query.Pair ("ServiceUpdateDescription", String.to_query f))
       ; Aws.Util.option_map v.service_update_status (fun f ->
             Aws.Query.Pair ("ServiceUpdateStatus", ServiceUpdateStatus.to_query f))
       ; Aws.Util.option_map v.service_update_recommended_apply_by_date (fun f ->
             Aws.Query.Pair ("ServiceUpdateRecommendedApplyByDate", DateTime.to_query f))
       ; Aws.Util.option_map v.service_update_severity (fun f ->
             Aws.Query.Pair ("ServiceUpdateSeverity", ServiceUpdateSeverity.to_query f))
       ; Aws.Util.option_map v.service_update_end_date (fun f ->
             Aws.Query.Pair ("ServiceUpdateEndDate", DateTime.to_query f))
       ; Aws.Util.option_map v.service_update_release_date (fun f ->
             Aws.Query.Pair ("ServiceUpdateReleaseDate", DateTime.to_query f))
       ; Aws.Util.option_map v.service_update_name (fun f ->
             Aws.Query.Pair ("ServiceUpdateName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.estimated_update_time (fun f ->
             "EstimatedUpdateTime", String.to_json f)
       ; Aws.Util.option_map v.auto_update_after_recommended_apply_by_date (fun f ->
             "AutoUpdateAfterRecommendedApplyByDate", Boolean.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.service_update_type (fun f ->
             "ServiceUpdateType", ServiceUpdateType.to_json f)
       ; Aws.Util.option_map v.service_update_description (fun f ->
             "ServiceUpdateDescription", String.to_json f)
       ; Aws.Util.option_map v.service_update_status (fun f ->
             "ServiceUpdateStatus", ServiceUpdateStatus.to_json f)
       ; Aws.Util.option_map v.service_update_recommended_apply_by_date (fun f ->
             "ServiceUpdateRecommendedApplyByDate", DateTime.to_json f)
       ; Aws.Util.option_map v.service_update_severity (fun f ->
             "ServiceUpdateSeverity", ServiceUpdateSeverity.to_json f)
       ; Aws.Util.option_map v.service_update_end_date (fun f ->
             "ServiceUpdateEndDate", DateTime.to_json f)
       ; Aws.Util.option_map v.service_update_release_date (fun f ->
             "ServiceUpdateReleaseDate", DateTime.to_json f)
       ; Aws.Util.option_map v.service_update_name (fun f ->
             "ServiceUpdateName", String.to_json f)
       ])

let of_json j =
  { service_update_name =
      Aws.Util.option_map (Aws.Json.lookup j "ServiceUpdateName") String.of_json
  ; service_update_release_date =
      Aws.Util.option_map (Aws.Json.lookup j "ServiceUpdateReleaseDate") DateTime.of_json
  ; service_update_end_date =
      Aws.Util.option_map (Aws.Json.lookup j "ServiceUpdateEndDate") DateTime.of_json
  ; service_update_severity =
      Aws.Util.option_map
        (Aws.Json.lookup j "ServiceUpdateSeverity")
        ServiceUpdateSeverity.of_json
  ; service_update_recommended_apply_by_date =
      Aws.Util.option_map
        (Aws.Json.lookup j "ServiceUpdateRecommendedApplyByDate")
        DateTime.of_json
  ; service_update_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "ServiceUpdateStatus")
        ServiceUpdateStatus.of_json
  ; service_update_description =
      Aws.Util.option_map (Aws.Json.lookup j "ServiceUpdateDescription") String.of_json
  ; service_update_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "ServiceUpdateType")
        ServiceUpdateType.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; auto_update_after_recommended_apply_by_date =
      Aws.Util.option_map
        (Aws.Json.lookup j "AutoUpdateAfterRecommendedApplyByDate")
        Boolean.of_json
  ; estimated_update_time =
      Aws.Util.option_map (Aws.Json.lookup j "EstimatedUpdateTime") String.of_json
  }
