open Aws.BaseTypes

type t =
  { addresses : AddressSet.t
  ; next_token : String.t option
  }

let make ?(addresses = []) ?next_token () = { addresses; next_token }

let parse xml =
  Some
    { addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "addressSet" xml) AddressSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("AddressSet", AddressSet.to_query v.addresses))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("addressSet", AddressSet.to_json v.addresses)
       ])

let of_json j =
  { addresses =
      AddressSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "addressSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
