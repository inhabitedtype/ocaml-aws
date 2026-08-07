open Aws.BaseTypes

type t =
  { service_principal : String.t option
  ; description : String.t option
  }

let make ?service_principal ?description () = { service_principal; description }

let parse xml =
  Some
    { service_principal =
        Aws.Util.option_bind (Aws.Xml.member "ServicePrincipal" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.service_principal (fun f ->
             Aws.Query.Pair ("ServicePrincipal", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.service_principal (fun f ->
             "ServicePrincipal", String.to_json f)
       ])

let of_json j =
  { service_principal =
      Aws.Util.option_map (Aws.Json.lookup j "ServicePrincipal") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  }
