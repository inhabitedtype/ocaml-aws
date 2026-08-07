open Aws.BaseTypes

type t =
  { error : UnsuccessfulItemError.t option
  ; resource_id : String.t option
  }

let make ?error ?resource_id () = { error; resource_id }

let parse xml =
  Some
    { error =
        Aws.Util.option_bind (Aws.Xml.member "error" xml) UnsuccessfulItemError.parse
    ; resource_id = Aws.Util.option_bind (Aws.Xml.member "resourceId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_id (fun f ->
             Aws.Query.Pair ("ResourceId", String.to_query f))
       ; Aws.Util.option_map v.error (fun f ->
             Aws.Query.Pair ("Error", UnsuccessfulItemError.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.resource_id (fun f -> "resourceId", String.to_json f)
       ; Aws.Util.option_map v.error (fun f -> "error", UnsuccessfulItemError.to_json f)
       ])

let of_json j =
  { error = Aws.Util.option_map (Aws.Json.lookup j "error") UnsuccessfulItemError.of_json
  ; resource_id = Aws.Util.option_map (Aws.Json.lookup j "resourceId") String.of_json
  }
