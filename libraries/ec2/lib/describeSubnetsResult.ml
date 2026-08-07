open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; subnets : SubnetList.t
  }

let make ?next_token ?(subnets = []) () = { next_token; subnets }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; subnets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "subnetSet" xml) SubnetList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("SubnetSet", SubnetList.to_query v.subnets))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("subnetSet", SubnetList.to_json v.subnets)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; subnets = SubnetList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "subnetSet"))
  }
