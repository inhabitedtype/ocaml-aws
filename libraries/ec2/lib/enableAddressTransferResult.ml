type t = { address_transfer : AddressTransfer.t option }

let make ?address_transfer () = { address_transfer }

let parse xml =
  Some
    { address_transfer =
        Aws.Util.option_bind (Aws.Xml.member "addressTransfer" xml) AddressTransfer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.address_transfer (fun f ->
             Aws.Query.Pair ("AddressTransfer", AddressTransfer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.address_transfer (fun f ->
             "addressTransfer", AddressTransfer.to_json f)
       ])

let of_json j =
  { address_transfer =
      Aws.Util.option_map (Aws.Json.lookup j "addressTransfer") AddressTransfer.of_json
  }
