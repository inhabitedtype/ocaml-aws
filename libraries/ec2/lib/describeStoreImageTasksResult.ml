open Aws.BaseTypes

type t =
  { store_image_task_results : StoreImageTaskResultSet.t
  ; next_token : String.t option
  }

let make ?(store_image_task_results = []) ?next_token () =
  { store_image_task_results; next_token }

let parse xml =
  Some
    { store_image_task_results =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "storeImageTaskResultSet" xml)
             StoreImageTaskResultSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "StoreImageTaskResultSet"
              , StoreImageTaskResultSet.to_query v.store_image_task_results ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "storeImageTaskResultSet"
           , StoreImageTaskResultSet.to_json v.store_image_task_results )
       ])

let of_json j =
  { store_image_task_results =
      StoreImageTaskResultSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "storeImageTaskResultSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
