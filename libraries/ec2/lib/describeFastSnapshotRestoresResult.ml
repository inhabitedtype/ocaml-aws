open Aws.BaseTypes

type t =
  { fast_snapshot_restores : DescribeFastSnapshotRestoreSuccessSet.t
  ; next_token : String.t option
  }

let make ?(fast_snapshot_restores = []) ?next_token () =
  { fast_snapshot_restores; next_token }

let parse xml =
  Some
    { fast_snapshot_restores =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "fastSnapshotRestoreSet" xml)
             DescribeFastSnapshotRestoreSuccessSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "FastSnapshotRestoreSet"
              , DescribeFastSnapshotRestoreSuccessSet.to_query v.fast_snapshot_restores ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "fastSnapshotRestoreSet"
           , DescribeFastSnapshotRestoreSuccessSet.to_json v.fast_snapshot_restores )
       ])

let of_json j =
  { fast_snapshot_restores =
      DescribeFastSnapshotRestoreSuccessSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "fastSnapshotRestoreSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
