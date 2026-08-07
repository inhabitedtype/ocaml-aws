open Aws.BaseTypes

type t = { cidr : String.t }

let make ~cidr () = { cidr }

let parse xml =
  Some
    { cidr =
        Aws.Xml.required
          "Cidr"
          (Aws.Util.option_bind (Aws.Xml.member "Cidr" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt [ Some (Aws.Query.Pair ("Cidr", String.to_query v.cidr)) ])

let to_json v = `Assoc (Aws.Util.list_filter_opt [ Some ("Cidr", String.to_json v.cidr) ])

let of_json j =
  { cidr = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Cidr")) }
