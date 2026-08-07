open Aws.BaseTypes

type t =
  { secondary_subnets : SecondarySubnetList.t
  ; next_token : String.t option
  }

let make ?(secondary_subnets = []) ?next_token () = { secondary_subnets; next_token }

let parse xml =
  Some
    { secondary_subnets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "secondarySubnetSet" xml)
             SecondarySubnetList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SecondarySubnetSet", SecondarySubnetList.to_query v.secondary_subnets))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("secondarySubnetSet", SecondarySubnetList.to_json v.secondary_subnets)
       ])

let of_json j =
  { secondary_subnets =
      SecondarySubnetList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "secondarySubnetSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
