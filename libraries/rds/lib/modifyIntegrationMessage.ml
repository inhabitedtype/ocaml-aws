open Aws.BaseTypes

type t =
  { integration_identifier : String.t
  ; integration_name : String.t option
  ; data_filter : String.t option
  ; description : String.t option
  }

let make ~integration_identifier ?integration_name ?data_filter ?description () =
  { integration_identifier; integration_name; data_filter; description }

let parse xml =
  Some
    { integration_identifier =
        Aws.Xml.required
          "IntegrationIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "IntegrationIdentifier" xml) String.parse)
    ; integration_name =
        Aws.Util.option_bind (Aws.Xml.member "IntegrationName" xml) String.parse
    ; data_filter = Aws.Util.option_bind (Aws.Xml.member "DataFilter" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.data_filter (fun f ->
             Aws.Query.Pair ("DataFilter", String.to_query f))
       ; Aws.Util.option_map v.integration_name (fun f ->
             Aws.Query.Pair ("IntegrationName", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("IntegrationIdentifier", String.to_query v.integration_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.data_filter (fun f -> "DataFilter", String.to_json f)
       ; Aws.Util.option_map v.integration_name (fun f ->
             "IntegrationName", String.to_json f)
       ; Some ("IntegrationIdentifier", String.to_json v.integration_identifier)
       ])

let of_json j =
  { integration_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IntegrationIdentifier"))
  ; integration_name =
      Aws.Util.option_map (Aws.Json.lookup j "IntegrationName") String.of_json
  ; data_filter = Aws.Util.option_map (Aws.Json.lookup j "DataFilter") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  }
