open Aws.BaseTypes

type t =
  { recommendation_id : String.t
  ; locale : String.t option
  ; status : String.t option
  ; recommended_action_updates : RecommendedActionUpdateList.t
  }

let make ~recommendation_id ?locale ?status ?(recommended_action_updates = []) () =
  { recommendation_id; locale; status; recommended_action_updates }

let parse xml =
  Some
    { recommendation_id =
        Aws.Xml.required
          "RecommendationId"
          (Aws.Util.option_bind (Aws.Xml.member "RecommendationId" xml) String.parse)
    ; locale = Aws.Util.option_bind (Aws.Xml.member "Locale" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; recommended_action_updates =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RecommendedActionUpdates" xml)
             RecommendedActionUpdateList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "RecommendedActionUpdates.member"
              , RecommendedActionUpdateList.to_query v.recommended_action_updates ))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.locale (fun f ->
             Aws.Query.Pair ("Locale", String.to_query f))
       ; Some (Aws.Query.Pair ("RecommendationId", String.to_query v.recommendation_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "RecommendedActionUpdates"
           , RecommendedActionUpdateList.to_json v.recommended_action_updates )
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.locale (fun f -> "Locale", String.to_json f)
       ; Some ("RecommendationId", String.to_json v.recommendation_id)
       ])

let of_json j =
  { recommendation_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RecommendationId"))
  ; locale = Aws.Util.option_map (Aws.Json.lookup j "Locale") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; recommended_action_updates =
      RecommendedActionUpdateList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RecommendedActionUpdates"))
  }
