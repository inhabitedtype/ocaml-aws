type t =
  { source : PathStatement.t option
  ; destination : PathStatement.t option
  ; through_resources : ThroughResourcesStatementList.t
  }

let make ?source ?destination ?(through_resources = []) () =
  { source; destination; through_resources }

let parse xml =
  Some
    { source = Aws.Util.option_bind (Aws.Xml.member "source" xml) PathStatement.parse
    ; destination =
        Aws.Util.option_bind (Aws.Xml.member "destination" xml) PathStatement.parse
    ; through_resources =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "throughResourceSet" xml)
             ThroughResourcesStatementList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ThroughResourceSet"
              , ThroughResourcesStatementList.to_query v.through_resources ))
       ; Aws.Util.option_map v.destination (fun f ->
             Aws.Query.Pair ("Destination", PathStatement.to_query f))
       ; Aws.Util.option_map v.source (fun f ->
             Aws.Query.Pair ("Source", PathStatement.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "throughResourceSet"
           , ThroughResourcesStatementList.to_json v.through_resources )
       ; Aws.Util.option_map v.destination (fun f ->
             "destination", PathStatement.to_json f)
       ; Aws.Util.option_map v.source (fun f -> "source", PathStatement.to_json f)
       ])

let of_json j =
  { source = Aws.Util.option_map (Aws.Json.lookup j "source") PathStatement.of_json
  ; destination =
      Aws.Util.option_map (Aws.Json.lookup j "destination") PathStatement.of_json
  ; through_resources =
      ThroughResourcesStatementList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "throughResourceSet"))
  }
