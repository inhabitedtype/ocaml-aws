open Aws.BaseTypes

type t =
  { d_b_proxy_endpoints : DBProxyEndpointList.t
  ; marker : String.t option
  }

let make ?(d_b_proxy_endpoints = []) ?marker () = { d_b_proxy_endpoints; marker }

let parse xml =
  Some
    { d_b_proxy_endpoints =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DBProxyEndpoints" xml)
             DBProxyEndpointList.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "DBProxyEndpoints.member"
              , DBProxyEndpointList.to_query v.d_b_proxy_endpoints ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some ("DBProxyEndpoints", DBProxyEndpointList.to_json v.d_b_proxy_endpoints)
       ])

let of_json j =
  { d_b_proxy_endpoints =
      DBProxyEndpointList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBProxyEndpoints"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
