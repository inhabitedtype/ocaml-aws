open Aws.BaseTypes

type t =
  { config : String.t option
  ; routes : VerifiedAccessInstanceOpenVpnClientConfigurationRouteList.t
  }

let make ?config ?(routes = []) () = { config; routes }

let parse xml =
  Some
    { config = Aws.Util.option_bind (Aws.Xml.member "config" xml) String.parse
    ; routes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "routeSet" xml)
             VerifiedAccessInstanceOpenVpnClientConfigurationRouteList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "RouteSet"
              , VerifiedAccessInstanceOpenVpnClientConfigurationRouteList.to_query
                  v.routes ))
       ; Aws.Util.option_map v.config (fun f ->
             Aws.Query.Pair ("Config", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "routeSet"
           , VerifiedAccessInstanceOpenVpnClientConfigurationRouteList.to_json v.routes )
       ; Aws.Util.option_map v.config (fun f -> "config", String.to_json f)
       ])

let of_json j =
  { config = Aws.Util.option_map (Aws.Json.lookup j "config") String.of_json
  ; routes =
      VerifiedAccessInstanceOpenVpnClientConfigurationRouteList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "routeSet"))
  }
