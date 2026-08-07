open Aws.BaseTypes

type t = { instance_type : String.t option }

let make ?instance_type () = { instance_type }

let parse xml =
  Some
    { instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_type (fun f -> "instanceType", String.to_json f) ])

let of_json j =
  { instance_type = Aws.Util.option_map (Aws.Json.lookup j "instanceType") String.of_json
  }
