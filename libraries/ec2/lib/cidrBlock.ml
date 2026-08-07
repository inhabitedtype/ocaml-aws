open Aws.BaseTypes

type t = { cidr_block : String.t option }

let make ?cidr_block () = { cidr_block }

let parse xml =
  Some { cidr_block = Aws.Util.option_bind (Aws.Xml.member "cidrBlock" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr_block (fun f ->
             Aws.Query.Pair ("CidrBlock", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr_block (fun f -> "cidrBlock", String.to_json f) ])

let of_json j =
  { cidr_block = Aws.Util.option_map (Aws.Json.lookup j "cidrBlock") String.of_json }
