type t =
  { source : PathStatementRequest.t option
  ; destination : PathStatementRequest.t option
  ; through_resources : ThroughResourcesStatementRequestList.t
  }

let make ?source ?destination ?(through_resources = []) () =
  { source; destination; through_resources }

let parse xml =
  Some
    { source =
        Aws.Util.option_bind (Aws.Xml.member "Source" xml) PathStatementRequest.parse
    ; destination =
        Aws.Util.option_bind (Aws.Xml.member "Destination" xml) PathStatementRequest.parse
    ; through_resources =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ThroughResource" xml)
             ThroughResourcesStatementRequestList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ThroughResource"
              , ThroughResourcesStatementRequestList.to_query v.through_resources ))
       ; Aws.Util.option_map v.destination (fun f ->
             Aws.Query.Pair ("Destination", PathStatementRequest.to_query f))
       ; Aws.Util.option_map v.source (fun f ->
             Aws.Query.Pair ("Source", PathStatementRequest.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "ThroughResource"
           , ThroughResourcesStatementRequestList.to_json v.through_resources )
       ; Aws.Util.option_map v.destination (fun f ->
             "Destination", PathStatementRequest.to_json f)
       ; Aws.Util.option_map v.source (fun f -> "Source", PathStatementRequest.to_json f)
       ])

let of_json j =
  { source = Aws.Util.option_map (Aws.Json.lookup j "Source") PathStatementRequest.of_json
  ; destination =
      Aws.Util.option_map (Aws.Json.lookup j "Destination") PathStatementRequest.of_json
  ; through_resources =
      ThroughResourcesStatementRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ThroughResource"))
  }
