open Aws.BaseTypes

type t =
  { export_image_tasks : ExportImageTaskList.t
  ; next_token : String.t option
  }

let make ?(export_image_tasks = []) ?next_token () = { export_image_tasks; next_token }

let parse xml =
  Some
    { export_image_tasks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "exportImageTaskSet" xml)
             ExportImageTaskList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ExportImageTaskSet", ExportImageTaskList.to_query v.export_image_tasks))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some ("exportImageTaskSet", ExportImageTaskList.to_json v.export_image_tasks)
       ])

let of_json j =
  { export_image_tasks =
      ExportImageTaskList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "exportImageTaskSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
