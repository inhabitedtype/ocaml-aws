open Aws.BaseTypes

type t = { resource_a_r_n : String.t }

let make ~resource_a_r_n () = { resource_a_r_n }

let parse xml =
  Some
    { resource_a_r_n =
        Aws.Xml.required
          "ResourceARN"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceARN" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ResourceARN", String.to_query v.resource_a_r_n)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("ResourceARN", String.to_json v.resource_a_r_n) ])

let of_json j =
  { resource_a_r_n =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceARN"))
  }
