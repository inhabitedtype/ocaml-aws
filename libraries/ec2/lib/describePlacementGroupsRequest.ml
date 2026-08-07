open Aws.BaseTypes

type t =
  { group_ids : PlacementGroupIdStringList.t
  ; dry_run : Boolean.t option
  ; group_names : PlacementGroupStringList.t
  ; filters : FilterList.t
  }

let make ?(group_ids = []) ?dry_run ?(group_names = []) ?(filters = []) () =
  { group_ids; dry_run; group_names; filters }

let parse xml =
  Some
    { group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "GroupId" xml)
             PlacementGroupIdStringList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; group_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "groupName" xml)
             PlacementGroupStringList.parse)
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Some
           (Aws.Query.Pair ("GroupName", PlacementGroupStringList.to_query v.group_names))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("GroupId", PlacementGroupIdStringList.to_query v.group_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filter", FilterList.to_json v.filters)
       ; Some ("groupName", PlacementGroupStringList.to_json v.group_names)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("GroupId", PlacementGroupIdStringList.to_json v.group_ids)
       ])

let of_json j =
  { group_ids =
      PlacementGroupIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GroupId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; group_names =
      PlacementGroupStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "groupName"))
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  }
