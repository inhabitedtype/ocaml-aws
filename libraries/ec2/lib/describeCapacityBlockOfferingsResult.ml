open Aws.BaseTypes

type t =
  { capacity_block_offerings : CapacityBlockOfferingSet.t
  ; next_token : String.t option
  }

let make ?(capacity_block_offerings = []) ?next_token () =
  { capacity_block_offerings; next_token }

let parse xml =
  Some
    { capacity_block_offerings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "capacityBlockOfferingSet" xml)
             CapacityBlockOfferingSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "CapacityBlockOfferingSet"
              , CapacityBlockOfferingSet.to_query v.capacity_block_offerings ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "capacityBlockOfferingSet"
           , CapacityBlockOfferingSet.to_json v.capacity_block_offerings )
       ])

let of_json j =
  { capacity_block_offerings =
      CapacityBlockOfferingSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "capacityBlockOfferingSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
