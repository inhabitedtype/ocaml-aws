open Aws.BaseTypes

type t =
  { load_balancer_name : String.t option
  ; tags : TagList.t
  }

let make ?load_balancer_name ?(tags = []) () = { load_balancer_name; tags }

let parse xml =
  Some
    { load_balancer_name =
        Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.load_balancer_name (fun f ->
             Aws.Query.Pair ("LoadBalancerName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.load_balancer_name (fun f ->
             "LoadBalancerName", String.to_json f)
       ])

let of_json j =
  { load_balancer_name =
      Aws.Util.option_map (Aws.Json.lookup j "LoadBalancerName") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
