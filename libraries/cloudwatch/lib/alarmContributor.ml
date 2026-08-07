open Aws.BaseTypes

type t =
  { contributor_id : String.t
  ; contributor_attributes : ContributorAttributes.t
  ; state_reason : String.t
  ; state_transitioned_timestamp : DateTime.t option
  }

let make
    ~contributor_id
    ~contributor_attributes
    ~state_reason
    ?state_transitioned_timestamp
    () =
  { contributor_id; contributor_attributes; state_reason; state_transitioned_timestamp }

let parse xml =
  Some
    { contributor_id =
        Aws.Xml.required
          "ContributorId"
          (Aws.Util.option_bind (Aws.Xml.member "ContributorId" xml) String.parse)
    ; contributor_attributes =
        Aws.Xml.required
          "ContributorAttributes"
          (Aws.Util.option_bind
             (Aws.Xml.member "ContributorAttributes" xml)
             ContributorAttributes.parse)
    ; state_reason =
        Aws.Xml.required
          "StateReason"
          (Aws.Util.option_bind (Aws.Xml.member "StateReason" xml) String.parse)
    ; state_transitioned_timestamp =
        Aws.Util.option_bind
          (Aws.Xml.member "StateTransitionedTimestamp" xml)
          DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state_transitioned_timestamp (fun f ->
             Aws.Query.Pair ("StateTransitionedTimestamp", DateTime.to_query f))
       ; Some (Aws.Query.Pair ("StateReason", String.to_query v.state_reason))
       ; Some
           (Aws.Query.Pair
              ( "ContributorAttributes"
              , ContributorAttributes.to_query v.contributor_attributes ))
       ; Some (Aws.Query.Pair ("ContributorId", String.to_query v.contributor_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.state_transitioned_timestamp (fun f ->
             "StateTransitionedTimestamp", DateTime.to_json f)
       ; Some ("StateReason", String.to_json v.state_reason)
       ; Some
           ( "ContributorAttributes"
           , ContributorAttributes.to_json v.contributor_attributes )
       ; Some ("ContributorId", String.to_json v.contributor_id)
       ])

let of_json j =
  { contributor_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ContributorId"))
  ; contributor_attributes =
      ContributorAttributes.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ContributorAttributes"))
  ; state_reason =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StateReason"))
  ; state_transitioned_timestamp =
      Aws.Util.option_map
        (Aws.Json.lookup j "StateTransitionedTimestamp")
        DateTime.of_json
  }
