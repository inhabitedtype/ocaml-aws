open Aws.BaseTypes

type t =
  { serve_signature : String.t option
  ; status_message : String.t option
  }

let make ?serve_signature ?status_message () = { serve_signature; status_message }

let parse xml =
  Some
    { serve_signature =
        Aws.Util.option_bind (Aws.Xml.member "ServeSignature" xml) String.parse
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "StatusMessage" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Aws.Util.option_map v.serve_signature (fun f ->
             Aws.Query.Pair ("ServeSignature", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status_message (fun f -> "StatusMessage", String.to_json f)
       ; Aws.Util.option_map v.serve_signature (fun f ->
             "ServeSignature", String.to_json f)
       ])

let of_json j =
  { serve_signature =
      Aws.Util.option_map (Aws.Json.lookup j "ServeSignature") String.of_json
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "StatusMessage") String.of_json
  }
