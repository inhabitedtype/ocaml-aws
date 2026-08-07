open Aws.BaseTypes

type t = { resource_name : String.t }

let make ~resource_name () = { resource_name }

let parse xml =
  Some
    { resource_name =
        Aws.Xml.required
          "ResourceName"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ResourceName", String.to_query v.resource_name)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("ResourceName", String.to_json v.resource_name) ])

let of_json j =
  { resource_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceName"))
  }
