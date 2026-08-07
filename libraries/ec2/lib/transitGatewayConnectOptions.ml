type t = { protocol : ProtocolValue.t option }

let make ?protocol () = { protocol }

let parse xml =
  Some
    { protocol = Aws.Util.option_bind (Aws.Xml.member "protocol" xml) ProtocolValue.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", ProtocolValue.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.protocol (fun f -> "protocol", ProtocolValue.to_json f) ])

let of_json j =
  { protocol = Aws.Util.option_map (Aws.Json.lookup j "protocol") ProtocolValue.of_json }
