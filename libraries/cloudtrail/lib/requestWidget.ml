open Aws.BaseTypes

type t =
  { query_statement : String.t
  ; query_parameters : QueryParameters.t
  ; view_properties : ViewPropertiesMap.t
  }

let make ~query_statement ?(query_parameters = []) ~view_properties () =
  { query_statement; query_parameters; view_properties }

let parse xml =
  Some
    { query_statement =
        Aws.Xml.required
          "QueryStatement"
          (Aws.Util.option_bind (Aws.Xml.member "QueryStatement" xml) String.parse)
    ; query_parameters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "QueryParameters" xml)
             QueryParameters.parse)
    ; view_properties =
        Aws.Xml.required
          "ViewProperties"
          (Aws.Util.option_bind
             (Aws.Xml.member "ViewProperties" xml)
             ViewPropertiesMap.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("ViewProperties", ViewPropertiesMap.to_query v.view_properties))
       ; Some
           (Aws.Query.Pair
              ("QueryParameters.member", QueryParameters.to_query v.query_parameters))
       ; Some (Aws.Query.Pair ("QueryStatement", String.to_query v.query_statement))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ViewProperties", ViewPropertiesMap.to_json v.view_properties)
       ; Some ("QueryParameters", QueryParameters.to_json v.query_parameters)
       ; Some ("QueryStatement", String.to_json v.query_statement)
       ])

let of_json j =
  { query_statement =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "QueryStatement"))
  ; query_parameters =
      QueryParameters.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "QueryParameters"))
  ; view_properties =
      ViewPropertiesMap.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ViewProperties"))
  }
