open Aws.BaseTypes

type t = { d_b_proxy_name : String.t }

let make ~d_b_proxy_name () = { d_b_proxy_name }

let parse xml =
  Some
    { d_b_proxy_name =
        Aws.Xml.required
          "DBProxyName"
          (Aws.Util.option_bind (Aws.Xml.member "DBProxyName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("DBProxyName", String.to_query v.d_b_proxy_name)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("DBProxyName", String.to_json v.d_b_proxy_name) ])

let of_json j =
  { d_b_proxy_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBProxyName"))
  }
