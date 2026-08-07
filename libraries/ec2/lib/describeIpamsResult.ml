open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; ipams : IpamSet.t
  }

let make ?next_token ?(ipams = []) () = { next_token; ipams }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; ipams =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ipamSet" xml) IpamSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("IpamSet", IpamSet.to_query v.ipams))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ipamSet", IpamSet.to_json v.ipams)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; ipams = IpamSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ipamSet"))
  }
