open Aws.BaseTypes

type t =
  { capacity_blocks : CapacityBlockSet.t
  ; next_token : String.t option
  }

let make ?(capacity_blocks = []) ?next_token () = { capacity_blocks; next_token }

let parse xml =
  Some
    { capacity_blocks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "capacityBlockSet" xml)
             CapacityBlockSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("CapacityBlockSet", CapacityBlockSet.to_query v.capacity_blocks))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("capacityBlockSet", CapacityBlockSet.to_json v.capacity_blocks)
       ])

let of_json j =
  { capacity_blocks =
      CapacityBlockSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "capacityBlockSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
