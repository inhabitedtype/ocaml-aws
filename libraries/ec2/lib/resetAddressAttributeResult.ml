type t = { address : AddressAttribute.t option }

let make ?address () = { address }

let parse xml =
  Some
    { address = Aws.Util.option_bind (Aws.Xml.member "address" xml) AddressAttribute.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.address (fun f ->
             Aws.Query.Pair ("Address", AddressAttribute.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.address (fun f -> "address", AddressAttribute.to_json f) ])

let of_json j =
  { address = Aws.Util.option_map (Aws.Json.lookup j "address") AddressAttribute.of_json }
