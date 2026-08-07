open Aws.BaseTypes

type t =
  { instance_refreshes : InstanceRefreshes.t
  ; next_token : String.t option
  }

let make ?(instance_refreshes = []) ?next_token () = { instance_refreshes; next_token }

let parse xml =
  Some
    { instance_refreshes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceRefreshes" xml)
             InstanceRefreshes.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("InstanceRefreshes.member", InstanceRefreshes.to_query v.instance_refreshes))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("InstanceRefreshes", InstanceRefreshes.to_json v.instance_refreshes)
       ])

let of_json j =
  { instance_refreshes =
      InstanceRefreshes.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceRefreshes"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
