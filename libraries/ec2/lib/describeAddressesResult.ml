type t = { addresses : AddressList.t }

let make ?(addresses = []) () = { addresses }

let parse xml =
  Some
    { addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "addressesSet" xml) AddressList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("AddressesSet", AddressList.to_query v.addresses)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("addressesSet", AddressList.to_json v.addresses) ])

let of_json j =
  { addresses =
      AddressList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "addressesSet"))
  }
