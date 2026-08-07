type t = { protocol : ProtocolValue.t }

let make ~protocol () = { protocol }

let parse xml =
  Some
    { protocol =
        Aws.Xml.required
          "Protocol"
          (Aws.Util.option_bind (Aws.Xml.member "Protocol" xml) ProtocolValue.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Protocol", ProtocolValue.to_query v.protocol)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("Protocol", ProtocolValue.to_json v.protocol) ])

let of_json j =
  { protocol =
      ProtocolValue.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Protocol"))
  }
