open Aws.BaseTypes

type t =
  { name : String.t
  ; values : Values.t
  }

let make ~name ?(values = []) () = { name; values }

let parse xml =
  Some
    { name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; values =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Values" xml) Values.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Values.member", Values.to_query v.values))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Values", Values.to_json v.values); Some ("Name", String.to_json v.name) ])

let of_json j =
  { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; values = Values.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Values"))
  }
