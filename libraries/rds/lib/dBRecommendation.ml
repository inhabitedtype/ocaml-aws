open Aws.BaseTypes

type t =
  { recommendation_id : String.t option
  ; type_id : String.t option
  ; severity : String.t option
  ; resource_arn : String.t option
  ; status : String.t option
  ; created_time : DateTime.t option
  ; updated_time : DateTime.t option
  ; detection : String.t option
  ; recommendation : String.t option
  ; description : String.t option
  ; reason : String.t option
  ; recommended_actions : RecommendedActionList.t
  ; category : String.t option
  ; source : String.t option
  ; type_detection : String.t option
  ; type_recommendation : String.t option
  ; impact : String.t option
  ; additional_info : String.t option
  ; links : DocLinkList.t
  ; issue_details : IssueDetails.t option
  }

let make
    ?recommendation_id
    ?type_id
    ?severity
    ?resource_arn
    ?status
    ?created_time
    ?updated_time
    ?detection
    ?recommendation
    ?description
    ?reason
    ?(recommended_actions = [])
    ?category
    ?source
    ?type_detection
    ?type_recommendation
    ?impact
    ?additional_info
    ?(links = [])
    ?issue_details
    () =
  { recommendation_id
  ; type_id
  ; severity
  ; resource_arn
  ; status
  ; created_time
  ; updated_time
  ; detection
  ; recommendation
  ; description
  ; reason
  ; recommended_actions
  ; category
  ; source
  ; type_detection
  ; type_recommendation
  ; impact
  ; additional_info
  ; links
  ; issue_details
  }

let parse xml =
  Some
    { recommendation_id =
        Aws.Util.option_bind (Aws.Xml.member "RecommendationId" xml) String.parse
    ; type_id = Aws.Util.option_bind (Aws.Xml.member "TypeId" xml) String.parse
    ; severity = Aws.Util.option_bind (Aws.Xml.member "Severity" xml) String.parse
    ; resource_arn = Aws.Util.option_bind (Aws.Xml.member "ResourceArn" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; created_time =
        Aws.Util.option_bind (Aws.Xml.member "CreatedTime" xml) DateTime.parse
    ; updated_time =
        Aws.Util.option_bind (Aws.Xml.member "UpdatedTime" xml) DateTime.parse
    ; detection = Aws.Util.option_bind (Aws.Xml.member "Detection" xml) String.parse
    ; recommendation =
        Aws.Util.option_bind (Aws.Xml.member "Recommendation" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; reason = Aws.Util.option_bind (Aws.Xml.member "Reason" xml) String.parse
    ; recommended_actions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RecommendedActions" xml)
             RecommendedActionList.parse)
    ; category = Aws.Util.option_bind (Aws.Xml.member "Category" xml) String.parse
    ; source = Aws.Util.option_bind (Aws.Xml.member "Source" xml) String.parse
    ; type_detection =
        Aws.Util.option_bind (Aws.Xml.member "TypeDetection" xml) String.parse
    ; type_recommendation =
        Aws.Util.option_bind (Aws.Xml.member "TypeRecommendation" xml) String.parse
    ; impact = Aws.Util.option_bind (Aws.Xml.member "Impact" xml) String.parse
    ; additional_info =
        Aws.Util.option_bind (Aws.Xml.member "AdditionalInfo" xml) String.parse
    ; links =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Links" xml) DocLinkList.parse)
    ; issue_details =
        Aws.Util.option_bind (Aws.Xml.member "IssueDetails" xml) IssueDetails.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.issue_details (fun f ->
             Aws.Query.Pair ("IssueDetails", IssueDetails.to_query f))
       ; Some (Aws.Query.Pair ("Links.member", DocLinkList.to_query v.links))
       ; Aws.Util.option_map v.additional_info (fun f ->
             Aws.Query.Pair ("AdditionalInfo", String.to_query f))
       ; Aws.Util.option_map v.impact (fun f ->
             Aws.Query.Pair ("Impact", String.to_query f))
       ; Aws.Util.option_map v.type_recommendation (fun f ->
             Aws.Query.Pair ("TypeRecommendation", String.to_query f))
       ; Aws.Util.option_map v.type_detection (fun f ->
             Aws.Query.Pair ("TypeDetection", String.to_query f))
       ; Aws.Util.option_map v.source (fun f ->
             Aws.Query.Pair ("Source", String.to_query f))
       ; Aws.Util.option_map v.category (fun f ->
             Aws.Query.Pair ("Category", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "RecommendedActions.member"
              , RecommendedActionList.to_query v.recommended_actions ))
       ; Aws.Util.option_map v.reason (fun f ->
             Aws.Query.Pair ("Reason", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.recommendation (fun f ->
             Aws.Query.Pair ("Recommendation", String.to_query f))
       ; Aws.Util.option_map v.detection (fun f ->
             Aws.Query.Pair ("Detection", String.to_query f))
       ; Aws.Util.option_map v.updated_time (fun f ->
             Aws.Query.Pair ("UpdatedTime", DateTime.to_query f))
       ; Aws.Util.option_map v.created_time (fun f ->
             Aws.Query.Pair ("CreatedTime", DateTime.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.resource_arn (fun f ->
             Aws.Query.Pair ("ResourceArn", String.to_query f))
       ; Aws.Util.option_map v.severity (fun f ->
             Aws.Query.Pair ("Severity", String.to_query f))
       ; Aws.Util.option_map v.type_id (fun f ->
             Aws.Query.Pair ("TypeId", String.to_query f))
       ; Aws.Util.option_map v.recommendation_id (fun f ->
             Aws.Query.Pair ("RecommendationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.issue_details (fun f ->
             "IssueDetails", IssueDetails.to_json f)
       ; Some ("Links", DocLinkList.to_json v.links)
       ; Aws.Util.option_map v.additional_info (fun f ->
             "AdditionalInfo", String.to_json f)
       ; Aws.Util.option_map v.impact (fun f -> "Impact", String.to_json f)
       ; Aws.Util.option_map v.type_recommendation (fun f ->
             "TypeRecommendation", String.to_json f)
       ; Aws.Util.option_map v.type_detection (fun f -> "TypeDetection", String.to_json f)
       ; Aws.Util.option_map v.source (fun f -> "Source", String.to_json f)
       ; Aws.Util.option_map v.category (fun f -> "Category", String.to_json f)
       ; Some ("RecommendedActions", RecommendedActionList.to_json v.recommended_actions)
       ; Aws.Util.option_map v.reason (fun f -> "Reason", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.recommendation (fun f ->
             "Recommendation", String.to_json f)
       ; Aws.Util.option_map v.detection (fun f -> "Detection", String.to_json f)
       ; Aws.Util.option_map v.updated_time (fun f -> "UpdatedTime", DateTime.to_json f)
       ; Aws.Util.option_map v.created_time (fun f -> "CreatedTime", DateTime.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.resource_arn (fun f -> "ResourceArn", String.to_json f)
       ; Aws.Util.option_map v.severity (fun f -> "Severity", String.to_json f)
       ; Aws.Util.option_map v.type_id (fun f -> "TypeId", String.to_json f)
       ; Aws.Util.option_map v.recommendation_id (fun f ->
             "RecommendationId", String.to_json f)
       ])

let of_json j =
  { recommendation_id =
      Aws.Util.option_map (Aws.Json.lookup j "RecommendationId") String.of_json
  ; type_id = Aws.Util.option_map (Aws.Json.lookup j "TypeId") String.of_json
  ; severity = Aws.Util.option_map (Aws.Json.lookup j "Severity") String.of_json
  ; resource_arn = Aws.Util.option_map (Aws.Json.lookup j "ResourceArn") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; created_time = Aws.Util.option_map (Aws.Json.lookup j "CreatedTime") DateTime.of_json
  ; updated_time = Aws.Util.option_map (Aws.Json.lookup j "UpdatedTime") DateTime.of_json
  ; detection = Aws.Util.option_map (Aws.Json.lookup j "Detection") String.of_json
  ; recommendation =
      Aws.Util.option_map (Aws.Json.lookup j "Recommendation") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; reason = Aws.Util.option_map (Aws.Json.lookup j "Reason") String.of_json
  ; recommended_actions =
      RecommendedActionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RecommendedActions"))
  ; category = Aws.Util.option_map (Aws.Json.lookup j "Category") String.of_json
  ; source = Aws.Util.option_map (Aws.Json.lookup j "Source") String.of_json
  ; type_detection =
      Aws.Util.option_map (Aws.Json.lookup j "TypeDetection") String.of_json
  ; type_recommendation =
      Aws.Util.option_map (Aws.Json.lookup j "TypeRecommendation") String.of_json
  ; impact = Aws.Util.option_map (Aws.Json.lookup j "Impact") String.of_json
  ; additional_info =
      Aws.Util.option_map (Aws.Json.lookup j "AdditionalInfo") String.of_json
  ; links = DocLinkList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Links"))
  ; issue_details =
      Aws.Util.option_map (Aws.Json.lookup j "IssueDetails") IssueDetails.of_json
  }
