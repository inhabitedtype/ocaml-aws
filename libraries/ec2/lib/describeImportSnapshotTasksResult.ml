open Aws.BaseTypes

type t =
  { import_snapshot_tasks : ImportSnapshotTaskList.t
  ; next_token : String.t option
  }

let make ?(import_snapshot_tasks = []) ?next_token () =
  { import_snapshot_tasks; next_token }

let parse xml =
  Some
    { import_snapshot_tasks =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "importSnapshotTaskSet" xml)
             ImportSnapshotTaskList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ImportSnapshotTaskSet"
              , ImportSnapshotTaskList.to_query v.import_snapshot_tasks ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "importSnapshotTaskSet"
           , ImportSnapshotTaskList.to_json v.import_snapshot_tasks )
       ])

let of_json j =
  { import_snapshot_tasks =
      ImportSnapshotTaskList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "importSnapshotTaskSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
