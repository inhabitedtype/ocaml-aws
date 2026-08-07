open Aws.BaseTypes

type t =
  { address_transfers : AddressTransferList.t
  ; next_token : String.t option
  }

let make ?(address_transfers = []) ?next_token () = { address_transfers; next_token }

let parse xml =
  Some
    { address_transfers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "addressTransferSet" xml)
             AddressTransferList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AddressTransferSet", AddressTransferList.to_query v.address_transfers))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("addressTransferSet", AddressTransferList.to_json v.address_transfers)
       ])

let of_json j =
  { address_transfers =
      AddressTransferList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "addressTransferSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
