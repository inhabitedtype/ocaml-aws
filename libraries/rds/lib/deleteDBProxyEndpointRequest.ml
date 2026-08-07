open Aws.BaseTypes

type t = { d_b_proxy_endpoint_name : String.t }

let make ~d_b_proxy_endpoint_name () = { d_b_proxy_endpoint_name }

let parse xml =
  Some
    { d_b_proxy_endpoint_name =
        Aws.Xml.required
          "DBProxyEndpointName"
          (Aws.Util.option_bind (Aws.Xml.member "DBProxyEndpointName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("DBProxyEndpointName", String.to_query v.d_b_proxy_endpoint_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("DBProxyEndpointName", String.to_json v.d_b_proxy_endpoint_name) ])

let of_json j =
  { d_b_proxy_endpoint_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBProxyEndpointName"))
  }
