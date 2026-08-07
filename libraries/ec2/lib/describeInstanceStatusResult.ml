open Aws.BaseTypes

type t =
  { instance_statuses : InstanceStatusList.t
  ; next_token : String.t option
  }

let make ?(instance_statuses = []) ?next_token () = { instance_statuses; next_token }

let parse xml =
  Some
    { instance_statuses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "instanceStatusSet" xml)
             InstanceStatusList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("InstanceStatusSet", InstanceStatusList.to_query v.instance_statuses))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("instanceStatusSet", InstanceStatusList.to_json v.instance_statuses)
       ])

let of_json j =
  { instance_statuses =
      InstanceStatusList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceStatusSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
