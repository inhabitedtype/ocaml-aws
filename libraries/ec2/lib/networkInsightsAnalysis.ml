open Aws.BaseTypes

type t =
  { network_insights_analysis_id : String.t option
  ; network_insights_analysis_arn : String.t option
  ; network_insights_path_id : String.t option
  ; additional_accounts : ValueStringList.t
  ; filter_in_arns : ArnList.t
  ; filter_out_arns : ArnList.t
  ; start_date : DateTime.t option
  ; status : AnalysisStatus.t option
  ; status_message : String.t option
  ; warning_message : String.t option
  ; network_path_found : Boolean.t option
  ; forward_path_components : PathComponentList.t
  ; return_path_components : PathComponentList.t
  ; explanations : ExplanationList.t
  ; alternate_path_hints : AlternatePathHintList.t
  ; suggested_accounts : ValueStringList.t
  ; tags : TagList.t
  }

let make
    ?network_insights_analysis_id
    ?network_insights_analysis_arn
    ?network_insights_path_id
    ?(additional_accounts = [])
    ?(filter_in_arns = [])
    ?(filter_out_arns = [])
    ?start_date
    ?status
    ?status_message
    ?warning_message
    ?network_path_found
    ?(forward_path_components = [])
    ?(return_path_components = [])
    ?(explanations = [])
    ?(alternate_path_hints = [])
    ?(suggested_accounts = [])
    ?(tags = [])
    () =
  { network_insights_analysis_id
  ; network_insights_analysis_arn
  ; network_insights_path_id
  ; additional_accounts
  ; filter_in_arns
  ; filter_out_arns
  ; start_date
  ; status
  ; status_message
  ; warning_message
  ; network_path_found
  ; forward_path_components
  ; return_path_components
  ; explanations
  ; alternate_path_hints
  ; suggested_accounts
  ; tags
  }

let parse xml =
  Some
    { network_insights_analysis_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInsightsAnalysisId" xml) String.parse
    ; network_insights_analysis_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInsightsAnalysisArn" xml)
          String.parse
    ; network_insights_path_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInsightsPathId" xml) String.parse
    ; additional_accounts =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "additionalAccountSet" xml)
             ValueStringList.parse)
    ; filter_in_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "filterInArnSet" xml) ArnList.parse)
    ; filter_out_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "filterOutArnSet" xml) ArnList.parse)
    ; start_date = Aws.Util.option_bind (Aws.Xml.member "startDate" xml) DateTime.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) AnalysisStatus.parse
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "statusMessage" xml) String.parse
    ; warning_message =
        Aws.Util.option_bind (Aws.Xml.member "warningMessage" xml) String.parse
    ; network_path_found =
        Aws.Util.option_bind (Aws.Xml.member "networkPathFound" xml) Boolean.parse
    ; forward_path_components =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "forwardPathComponentSet" xml)
             PathComponentList.parse)
    ; return_path_components =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "returnPathComponentSet" xml)
             PathComponentList.parse)
    ; explanations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "explanationSet" xml)
             ExplanationList.parse)
    ; alternate_path_hints =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "alternatePathHintSet" xml)
             AlternatePathHintList.parse)
    ; suggested_accounts =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "suggestedAccountSet" xml)
             ValueStringList.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ("SuggestedAccountSet", ValueStringList.to_query v.suggested_accounts))
       ; Some
           (Aws.Query.Pair
              ( "AlternatePathHintSet"
              , AlternatePathHintList.to_query v.alternate_path_hints ))
       ; Some (Aws.Query.Pair ("ExplanationSet", ExplanationList.to_query v.explanations))
       ; Some
           (Aws.Query.Pair
              ( "ReturnPathComponentSet"
              , PathComponentList.to_query v.return_path_components ))
       ; Some
           (Aws.Query.Pair
              ( "ForwardPathComponentSet"
              , PathComponentList.to_query v.forward_path_components ))
       ; Aws.Util.option_map v.network_path_found (fun f ->
             Aws.Query.Pair ("NetworkPathFound", Boolean.to_query f))
       ; Aws.Util.option_map v.warning_message (fun f ->
             Aws.Query.Pair ("WarningMessage", String.to_query f))
       ; Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", AnalysisStatus.to_query f))
       ; Aws.Util.option_map v.start_date (fun f ->
             Aws.Query.Pair ("StartDate", DateTime.to_query f))
       ; Some (Aws.Query.Pair ("FilterOutArnSet", ArnList.to_query v.filter_out_arns))
       ; Some (Aws.Query.Pair ("FilterInArnSet", ArnList.to_query v.filter_in_arns))
       ; Some
           (Aws.Query.Pair
              ("AdditionalAccountSet", ValueStringList.to_query v.additional_accounts))
       ; Aws.Util.option_map v.network_insights_path_id (fun f ->
             Aws.Query.Pair ("NetworkInsightsPathId", String.to_query f))
       ; Aws.Util.option_map v.network_insights_analysis_arn (fun f ->
             Aws.Query.Pair ("NetworkInsightsAnalysisArn", String.to_query f))
       ; Aws.Util.option_map v.network_insights_analysis_id (fun f ->
             Aws.Query.Pair ("NetworkInsightsAnalysisId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Some ("suggestedAccountSet", ValueStringList.to_json v.suggested_accounts)
       ; Some
           ("alternatePathHintSet", AlternatePathHintList.to_json v.alternate_path_hints)
       ; Some ("explanationSet", ExplanationList.to_json v.explanations)
       ; Some
           ("returnPathComponentSet", PathComponentList.to_json v.return_path_components)
       ; Some
           ("forwardPathComponentSet", PathComponentList.to_json v.forward_path_components)
       ; Aws.Util.option_map v.network_path_found (fun f ->
             "networkPathFound", Boolean.to_json f)
       ; Aws.Util.option_map v.warning_message (fun f ->
             "warningMessage", String.to_json f)
       ; Aws.Util.option_map v.status_message (fun f -> "statusMessage", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "status", AnalysisStatus.to_json f)
       ; Aws.Util.option_map v.start_date (fun f -> "startDate", DateTime.to_json f)
       ; Some ("filterOutArnSet", ArnList.to_json v.filter_out_arns)
       ; Some ("filterInArnSet", ArnList.to_json v.filter_in_arns)
       ; Some ("additionalAccountSet", ValueStringList.to_json v.additional_accounts)
       ; Aws.Util.option_map v.network_insights_path_id (fun f ->
             "networkInsightsPathId", String.to_json f)
       ; Aws.Util.option_map v.network_insights_analysis_arn (fun f ->
             "networkInsightsAnalysisArn", String.to_json f)
       ; Aws.Util.option_map v.network_insights_analysis_id (fun f ->
             "networkInsightsAnalysisId", String.to_json f)
       ])

let of_json j =
  { network_insights_analysis_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInsightsAnalysisId") String.of_json
  ; network_insights_analysis_arn =
      Aws.Util.option_map (Aws.Json.lookup j "networkInsightsAnalysisArn") String.of_json
  ; network_insights_path_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInsightsPathId") String.of_json
  ; additional_accounts =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "additionalAccountSet"))
  ; filter_in_arns =
      ArnList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "filterInArnSet"))
  ; filter_out_arns =
      ArnList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "filterOutArnSet"))
  ; start_date = Aws.Util.option_map (Aws.Json.lookup j "startDate") DateTime.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") AnalysisStatus.of_json
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "statusMessage") String.of_json
  ; warning_message =
      Aws.Util.option_map (Aws.Json.lookup j "warningMessage") String.of_json
  ; network_path_found =
      Aws.Util.option_map (Aws.Json.lookup j "networkPathFound") Boolean.of_json
  ; forward_path_components =
      PathComponentList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "forwardPathComponentSet"))
  ; return_path_components =
      PathComponentList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "returnPathComponentSet"))
  ; explanations =
      ExplanationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "explanationSet"))
  ; alternate_path_hints =
      AlternatePathHintList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "alternatePathHintSet"))
  ; suggested_accounts =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "suggestedAccountSet"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
