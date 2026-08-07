open Aws.BaseTypes

type t =
  { load_balancer_names : LoadBalancerNames.t
  ; marker : String.t option
  ; page_size : Integer.t option
  }

let make ?(load_balancer_names = []) ?marker ?page_size () =
  { load_balancer_names; marker; page_size }

let parse xml =
  Some
    { load_balancer_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LoadBalancerNames" xml)
             LoadBalancerNames.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; page_size = Aws.Util.option_bind (Aws.Xml.member "PageSize" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.page_size (fun f ->
             Aws.Query.Pair ("PageSize", Integer.to_query f))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LoadBalancerNames.member"
              , LoadBalancerNames.to_query v.load_balancer_names ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.page_size (fun f -> "PageSize", Integer.to_json f)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some ("LoadBalancerNames", LoadBalancerNames.to_json v.load_balancer_names)
       ])

let of_json j =
  { load_balancer_names =
      LoadBalancerNames.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerNames"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; page_size = Aws.Util.option_map (Aws.Json.lookup j "PageSize") Integer.of_json
  }
