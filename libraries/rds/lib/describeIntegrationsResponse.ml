open Aws.BaseTypes

type t =
  { marker : String.t option
  ; integrations : IntegrationList.t
  }

let make ?marker ?(integrations = []) () = { marker; integrations }

let parse xml =
  Some
    { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; integrations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Integrations" xml) IntegrationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("Integrations.member", IntegrationList.to_query v.integrations))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Integrations", IntegrationList.to_json v.integrations)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ])

let of_json j =
  { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; integrations =
      IntegrationList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Integrations"))
  }
