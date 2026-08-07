open Aws.BaseTypes

type t = { idle_timeout : Integer.t }

let make ~idle_timeout () = { idle_timeout }

let parse xml =
  Some
    { idle_timeout =
        Aws.Xml.required
          "IdleTimeout"
          (Aws.Util.option_bind (Aws.Xml.member "IdleTimeout" xml) Integer.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("IdleTimeout", Integer.to_query v.idle_timeout)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("IdleTimeout", Integer.to_json v.idle_timeout) ])

let of_json j =
  { idle_timeout =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IdleTimeout"))
  }
