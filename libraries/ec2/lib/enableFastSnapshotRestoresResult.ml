type t =
  { successful : EnableFastSnapshotRestoreSuccessSet.t
  ; unsuccessful : EnableFastSnapshotRestoreErrorSet.t
  }

let make ?(successful = []) ?(unsuccessful = []) () = { successful; unsuccessful }

let parse xml =
  Some
    { successful =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "successful" xml)
             EnableFastSnapshotRestoreSuccessSet.parse)
    ; unsuccessful =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "unsuccessful" xml)
             EnableFastSnapshotRestoreErrorSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("Unsuccessful", EnableFastSnapshotRestoreErrorSet.to_query v.unsuccessful))
       ; Some
           (Aws.Query.Pair
              ("Successful", EnableFastSnapshotRestoreSuccessSet.to_query v.successful))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("unsuccessful", EnableFastSnapshotRestoreErrorSet.to_json v.unsuccessful)
       ; Some ("successful", EnableFastSnapshotRestoreSuccessSet.to_json v.successful)
       ])

let of_json j =
  { successful =
      EnableFastSnapshotRestoreSuccessSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "successful"))
  ; unsuccessful =
      EnableFastSnapshotRestoreErrorSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "unsuccessful"))
  }
