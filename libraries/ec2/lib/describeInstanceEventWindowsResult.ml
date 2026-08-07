open Aws.BaseTypes

type t =
  { instance_event_windows : InstanceEventWindowSet.t
  ; next_token : String.t option
  }

let make ?(instance_event_windows = []) ?next_token () =
  { instance_event_windows; next_token }

let parse xml =
  Some
    { instance_event_windows =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "instanceEventWindowSet" xml)
             InstanceEventWindowSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "InstanceEventWindowSet"
              , InstanceEventWindowSet.to_query v.instance_event_windows ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "instanceEventWindowSet"
           , InstanceEventWindowSet.to_json v.instance_event_windows )
       ])

let of_json j =
  { instance_event_windows =
      InstanceEventWindowSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceEventWindowSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
