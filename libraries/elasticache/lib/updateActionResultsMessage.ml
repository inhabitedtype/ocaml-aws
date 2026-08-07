type t =
  { processed_update_actions : ProcessedUpdateActionList.t
  ; unprocessed_update_actions : UnprocessedUpdateActionList.t
  }

let make ?(processed_update_actions = []) ?(unprocessed_update_actions = []) () =
  { processed_update_actions; unprocessed_update_actions }

let parse xml =
  Some
    { processed_update_actions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ProcessedUpdateActions" xml)
             ProcessedUpdateActionList.parse)
    ; unprocessed_update_actions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "UnprocessedUpdateActions" xml)
             UnprocessedUpdateActionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "UnprocessedUpdateActions.member"
              , UnprocessedUpdateActionList.to_query v.unprocessed_update_actions ))
       ; Some
           (Aws.Query.Pair
              ( "ProcessedUpdateActions.member"
              , ProcessedUpdateActionList.to_query v.processed_update_actions ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "UnprocessedUpdateActions"
           , UnprocessedUpdateActionList.to_json v.unprocessed_update_actions )
       ; Some
           ( "ProcessedUpdateActions"
           , ProcessedUpdateActionList.to_json v.processed_update_actions )
       ])

let of_json j =
  { processed_update_actions =
      ProcessedUpdateActionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ProcessedUpdateActions"))
  ; unprocessed_update_actions =
      UnprocessedUpdateActionList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "UnprocessedUpdateActions"))
  }
