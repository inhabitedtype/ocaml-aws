open Aws.BaseTypes

type t = { process_name : String.t }

let make ~process_name () = { process_name }

let parse xml =
  Some
    { process_name =
        Aws.Xml.required
          "ProcessName"
          (Aws.Util.option_bind (Aws.Xml.member "ProcessName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ProcessName", String.to_query v.process_name)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("ProcessName", String.to_json v.process_name) ])

let of_json j =
  { process_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ProcessName"))
  }
