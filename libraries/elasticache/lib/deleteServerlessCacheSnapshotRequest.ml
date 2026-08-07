open Aws.BaseTypes

type t = { serverless_cache_snapshot_name : String.t }

let make ~serverless_cache_snapshot_name () = { serverless_cache_snapshot_name }

let parse xml =
  Some
    { serverless_cache_snapshot_name =
        Aws.Xml.required
          "ServerlessCacheSnapshotName"
          (Aws.Util.option_bind
             (Aws.Xml.member "ServerlessCacheSnapshotName" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ServerlessCacheSnapshotName"
              , String.to_query v.serverless_cache_snapshot_name ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("ServerlessCacheSnapshotName", String.to_json v.serverless_cache_snapshot_name)
       ])

let of_json j =
  { serverless_cache_snapshot_name =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ServerlessCacheSnapshotName"))
  }
