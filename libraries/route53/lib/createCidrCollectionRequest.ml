open Aws.BaseTypes

type t =
  { name : String.t
  ; caller_reference : String.t
  }

let make ~name ~caller_reference () = { name; caller_reference }

let parse xml =
  Some
    { name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; caller_reference =
        Aws.Xml.required
          "CallerReference"
          (Aws.Util.option_bind (Aws.Xml.member "CallerReference" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("CallerReference", String.to_query v.caller_reference))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("CallerReference", String.to_json v.caller_reference)
       ; Some ("Name", String.to_json v.name)
       ])

let of_json j =
  { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; caller_reference =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CallerReference"))
  }
