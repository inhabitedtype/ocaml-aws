open Aws.BaseTypes

type t = { ipv6_cidr_block : String.t option }

let make ?ipv6_cidr_block () = { ipv6_cidr_block }

let parse xml =
  Some
    { ipv6_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "ipv6CidrBlock" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipv6_cidr_block (fun f ->
             Aws.Query.Pair ("Ipv6CidrBlock", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipv6_cidr_block (fun f ->
             "ipv6CidrBlock", String.to_json f)
       ])

let of_json j =
  { ipv6_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "ipv6CidrBlock") String.of_json
  }
