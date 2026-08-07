open Aws.BaseTypes

type t = { instance_family : String.t option }

let make ?instance_family () = { instance_family }

let parse xml =
  Some
    { instance_family =
        Aws.Util.option_bind (Aws.Xml.member "instanceFamily" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_family (fun f ->
             Aws.Query.Pair ("InstanceFamily", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_family (fun f ->
             "instanceFamily", String.to_json f)
       ])

let of_json j =
  { instance_family =
      Aws.Util.option_map (Aws.Json.lookup j "instanceFamily") String.of_json
  }
