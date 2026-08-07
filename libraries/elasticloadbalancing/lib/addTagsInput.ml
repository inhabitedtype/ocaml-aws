type t =
  { load_balancer_names : LoadBalancerNames.t
  ; tags : TagList.t
  }

let make ~load_balancer_names ~tags () = { load_balancer_names; tags }

let parse xml =
  Some
    { load_balancer_names =
        Aws.Xml.required
          "LoadBalancerNames"
          (Aws.Util.option_bind
             (Aws.Xml.member "LoadBalancerNames" xml)
             LoadBalancerNames.parse)
    ; tags =
        Aws.Xml.required
          "Tags"
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ( "LoadBalancerNames.member"
              , LoadBalancerNames.to_query v.load_balancer_names ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Some ("LoadBalancerNames", LoadBalancerNames.to_json v.load_balancer_names)
       ])

let of_json j =
  { load_balancer_names =
      LoadBalancerNames.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerNames"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
