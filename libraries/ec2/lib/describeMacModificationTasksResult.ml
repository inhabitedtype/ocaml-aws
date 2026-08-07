open Aws.BaseTypes

type t =
  { mac_modification_tasks : MacModificationTaskList.t
  ; next_token : String.t option
  }

let make ?(mac_modification_tasks = []) ?next_token () =
  { mac_modification_tasks; next_token }

let parse xml =
  Some
    { mac_modification_tasks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "macModificationTaskSet" xml)
             MacModificationTaskList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "MacModificationTaskSet"
              , MacModificationTaskList.to_query v.mac_modification_tasks ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "macModificationTaskSet"
           , MacModificationTaskList.to_json v.mac_modification_tasks )
       ])

let of_json j =
  { mac_modification_tasks =
      MacModificationTaskList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "macModificationTaskSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
