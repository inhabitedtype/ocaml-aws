open Aws.BaseTypes

type t =
  { code : ClientVpnEndpointStatusCode.t option
  ; message : String.t option
  }

let make ?code ?message () = { code; message }

let parse xml =
  Some
    { code =
        Aws.Util.option_bind (Aws.Xml.member "code" xml) ClientVpnEndpointStatusCode.parse
    ; message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.message (fun f ->
             Aws.Query.Pair ("Message", String.to_query f))
       ; Aws.Util.option_map v.code (fun f ->
             Aws.Query.Pair ("Code", ClientVpnEndpointStatusCode.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f)
       ; Aws.Util.option_map v.code (fun f ->
             "code", ClientVpnEndpointStatusCode.to_json f)
       ])

let of_json j =
  { code =
      Aws.Util.option_map (Aws.Json.lookup j "code") ClientVpnEndpointStatusCode.of_json
  ; message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json
  }
