type t =
  { successful : DisableFastSnapshotRestoreSuccessSet.t
  ; unsuccessful : DisableFastSnapshotRestoreErrorSet.t
  }

let make ?(successful = []) ?(unsuccessful = []) () = { successful; unsuccessful }

let parse xml =
  Some
    { successful =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "successful" xml)
             DisableFastSnapshotRestoreSuccessSet.parse)
    ; unsuccessful =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "unsuccessful" xml)
             DisableFastSnapshotRestoreErrorSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("Unsuccessful", DisableFastSnapshotRestoreErrorSet.to_query v.unsuccessful))
       ; Some
           (Aws.Query.Pair
              ("Successful", DisableFastSnapshotRestoreSuccessSet.to_query v.successful))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("unsuccessful", DisableFastSnapshotRestoreErrorSet.to_json v.unsuccessful)
       ; Some ("successful", DisableFastSnapshotRestoreSuccessSet.to_json v.successful)
       ])

let of_json j =
  { successful =
      DisableFastSnapshotRestoreSuccessSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "successful"))
  ; unsuccessful =
      DisableFastSnapshotRestoreErrorSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "unsuccessful"))
  }
