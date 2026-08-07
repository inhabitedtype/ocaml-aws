open Aws.BaseTypes

type t =
  { secondary_networks : SecondaryNetworkList.t
  ; next_token : String.t option
  }

let make ?(secondary_networks = []) ?next_token () = { secondary_networks; next_token }

let parse xml =
  Some
    { secondary_networks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "secondaryNetworkSet" xml)
             SecondaryNetworkList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SecondaryNetworkSet", SecondaryNetworkList.to_query v.secondary_networks))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("secondaryNetworkSet", SecondaryNetworkList.to_json v.secondary_networks)
       ])

let of_json j =
  { secondary_networks =
      SecondaryNetworkList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "secondaryNetworkSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
