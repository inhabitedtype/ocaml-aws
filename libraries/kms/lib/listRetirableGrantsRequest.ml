open Aws.BaseTypes

type t =
  { limit : Integer.t option
  ; marker : String.t option
  ; retiring_principal : String.t option
  ; retiring_service_principal : String.t option
  }

let make ?limit ?marker ?retiring_principal ?retiring_service_principal () =
  { limit; marker; retiring_principal; retiring_service_principal }

let parse xml =
  Some
    { limit = Aws.Util.option_bind (Aws.Xml.member "Limit" xml) Integer.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; retiring_principal =
        Aws.Util.option_bind (Aws.Xml.member "RetiringPrincipal" xml) String.parse
    ; retiring_service_principal =
        Aws.Util.option_bind (Aws.Xml.member "RetiringServicePrincipal" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.retiring_service_principal (fun f ->
             Aws.Query.Pair ("RetiringServicePrincipal", String.to_query f))
       ; Aws.Util.option_map v.retiring_principal (fun f ->
             Aws.Query.Pair ("RetiringPrincipal", String.to_query f))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.limit (fun f ->
             Aws.Query.Pair ("Limit", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.retiring_service_principal (fun f ->
             "RetiringServicePrincipal", String.to_json f)
       ; Aws.Util.option_map v.retiring_principal (fun f ->
             "RetiringPrincipal", String.to_json f)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.limit (fun f -> "Limit", Integer.to_json f)
       ])

let of_json j =
  { limit = Aws.Util.option_map (Aws.Json.lookup j "Limit") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; retiring_principal =
      Aws.Util.option_map (Aws.Json.lookup j "RetiringPrincipal") String.of_json
  ; retiring_service_principal =
      Aws.Util.option_map (Aws.Json.lookup j "RetiringServicePrincipal") String.of_json
  }
