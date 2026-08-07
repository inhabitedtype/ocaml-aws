open Aws.BaseTypes

type t = { client_configuration : String.t option }

let make ?client_configuration () = { client_configuration }

let parse xml =
  Some
    { client_configuration =
        Aws.Util.option_bind (Aws.Xml.member "clientConfiguration" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_configuration (fun f ->
             Aws.Query.Pair ("ClientConfiguration", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_configuration (fun f ->
             "clientConfiguration", String.to_json f)
       ])

let of_json j =
  { client_configuration =
      Aws.Util.option_map (Aws.Json.lookup j "clientConfiguration") String.of_json
  }
