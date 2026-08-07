open Aws.BaseTypes

type t =
  { marker : String.t option
  ; parameters : ParametersList.t
  ; cache_node_type_specific_parameters : CacheNodeTypeSpecificParametersList.t
  }

let make ?marker ?(parameters = []) ?(cache_node_type_specific_parameters = []) () =
  { marker; parameters; cache_node_type_specific_parameters }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; parameters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) ParametersList.parse)
    ; cache_node_type_specific_parameters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheNodeTypeSpecificParameters" xml)
             CacheNodeTypeSpecificParametersList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "CacheNodeTypeSpecificParameters.member"
              , CacheNodeTypeSpecificParametersList.to_query
                  v.cache_node_type_specific_parameters ))
       ; Some (Aws.Query.Pair ("Parameters.member", ParametersList.to_query v.parameters))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "CacheNodeTypeSpecificParameters"
           , CacheNodeTypeSpecificParametersList.to_json
               v.cache_node_type_specific_parameters )
       ; Some ("Parameters", ParametersList.to_json v.parameters)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; parameters =
      ParametersList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
  ; cache_node_type_specific_parameters =
      CacheNodeTypeSpecificParametersList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheNodeTypeSpecificParameters"))
  }
