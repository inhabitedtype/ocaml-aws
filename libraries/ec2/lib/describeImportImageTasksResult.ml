open Aws.BaseTypes

type t =
  { import_image_tasks : ImportImageTaskList.t
  ; next_token : String.t option
  }

let make ?(import_image_tasks = []) ?next_token () = { import_image_tasks; next_token }

let parse xml =
  Some
    { import_image_tasks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "importImageTaskSet" xml)
             ImportImageTaskList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ImportImageTaskSet", ImportImageTaskList.to_query v.import_image_tasks))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("importImageTaskSet", ImportImageTaskList.to_json v.import_image_tasks)
       ])

let of_json j =
  { import_image_tasks =
      ImportImageTaskList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "importImageTaskSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
