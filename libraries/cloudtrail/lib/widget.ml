open Aws.BaseTypes

type t =
  { query_alias : String.t option
  ; query_statement : String.t option
  ; query_parameters : QueryParameters.t
  ; view_properties : ViewPropertiesMap.t option
  }

let make ?query_alias ?query_statement ?(query_parameters = []) ?view_properties () =
  { query_alias; query_statement; query_parameters; view_properties }

let parse xml =
  Some
    { query_alias = Aws.Util.option_bind (Aws.Xml.member "QueryAlias" xml) String.parse
    ; query_statement =
        Aws.Util.option_bind (Aws.Xml.member "QueryStatement" xml) String.parse
    ; query_parameters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "QueryParameters" xml)
             QueryParameters.parse)
    ; view_properties =
        Aws.Util.option_bind (Aws.Xml.member "ViewProperties" xml) ViewPropertiesMap.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.view_properties (fun f ->
             Aws.Query.Pair ("ViewProperties", ViewPropertiesMap.to_query f))
       ; Some
           (Aws.Query.Pair
              ("QueryParameters.member", QueryParameters.to_query v.query_parameters))
       ; Aws.Util.option_map v.query_statement (fun f ->
             Aws.Query.Pair ("QueryStatement", String.to_query f))
       ; Aws.Util.option_map v.query_alias (fun f ->
             Aws.Query.Pair ("QueryAlias", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.view_properties (fun f ->
             "ViewProperties", ViewPropertiesMap.to_json f)
       ; Some ("QueryParameters", QueryParameters.to_json v.query_parameters)
       ; Aws.Util.option_map v.query_statement (fun f ->
             "QueryStatement", String.to_json f)
       ; Aws.Util.option_map v.query_alias (fun f -> "QueryAlias", String.to_json f)
       ])

let of_json j =
  { query_alias = Aws.Util.option_map (Aws.Json.lookup j "QueryAlias") String.of_json
  ; query_statement =
      Aws.Util.option_map (Aws.Json.lookup j "QueryStatement") String.of_json
  ; query_parameters =
      QueryParameters.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "QueryParameters"))
  ; view_properties =
      Aws.Util.option_map (Aws.Json.lookup j "ViewProperties") ViewPropertiesMap.of_json
  }
