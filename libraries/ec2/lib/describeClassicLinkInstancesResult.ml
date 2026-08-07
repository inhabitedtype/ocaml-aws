open Aws.BaseTypes

type t =
  { instances : ClassicLinkInstanceList.t
  ; next_token : String.t option
  }

let make ?(instances = []) ?next_token () = { instances; next_token }

let parse xml =
  Some
    { instances =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "instancesSet" xml)
             ClassicLinkInstanceList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair ("InstancesSet", ClassicLinkInstanceList.to_query v.instances))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("instancesSet", ClassicLinkInstanceList.to_json v.instances)
       ])

let of_json j =
  { instances =
      ClassicLinkInstanceList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instancesSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
