open Aws.BaseTypes

type t =
  { instances : RegisteredInstanceList.t
  ; next_token : String.t option
  }

let make ?(instances = []) ?next_token () = { instances; next_token }

let parse xml =
  Some
    { instances =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "instanceSet" xml)
             RegisteredInstanceList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair ("InstanceSet", RegisteredInstanceList.to_query v.instances))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("instanceSet", RegisteredInstanceList.to_json v.instances)
       ])

let of_json j =
  { instances =
      RegisteredInstanceList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
