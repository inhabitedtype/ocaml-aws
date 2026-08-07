open Aws.BaseTypes

type t =
  { executable_users : ExecutableByStringList.t
  ; image_ids : ImageIdStringList.t
  ; owners : OwnerStringList.t
  ; include_deprecated : Boolean.t option
  ; include_disabled : Boolean.t option
  ; max_results : Integer.t option
  ; next_token : String.t option
  ; dry_run : Boolean.t option
  ; filters : FilterList.t
  }

let make
    ?(executable_users = [])
    ?(image_ids = [])
    ?(owners = [])
    ?include_deprecated
    ?include_disabled
    ?max_results
    ?next_token
    ?dry_run
    ?(filters = [])
    () =
  { executable_users
  ; image_ids
  ; owners
  ; include_deprecated
  ; include_disabled
  ; max_results
  ; next_token
  ; dry_run
  ; filters
  }

let parse xml =
  Some
    { executable_users =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ExecutableBy" xml)
             ExecutableByStringList.parse)
    ; image_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) ImageIdStringList.parse)
    ; owners =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Owner" xml) OwnerStringList.parse)
    ; include_deprecated =
        Aws.Util.option_bind (Aws.Xml.member "IncludeDeprecated" xml) Boolean.parse
    ; include_disabled =
        Aws.Util.option_bind (Aws.Xml.member "IncludeDisabled" xml) Boolean.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.include_disabled (fun f ->
             Aws.Query.Pair ("IncludeDisabled", Boolean.to_query f))
       ; Aws.Util.option_map v.include_deprecated (fun f ->
             Aws.Query.Pair ("IncludeDeprecated", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Owner", OwnerStringList.to_query v.owners))
       ; Some (Aws.Query.Pair ("ImageId", ImageIdStringList.to_query v.image_ids))
       ; Some
           (Aws.Query.Pair
              ("ExecutableBy", ExecutableByStringList.to_query v.executable_users))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.include_disabled (fun f ->
             "IncludeDisabled", Boolean.to_json f)
       ; Aws.Util.option_map v.include_deprecated (fun f ->
             "IncludeDeprecated", Boolean.to_json f)
       ; Some ("Owner", OwnerStringList.to_json v.owners)
       ; Some ("ImageId", ImageIdStringList.to_json v.image_ids)
       ; Some ("ExecutableBy", ExecutableByStringList.to_json v.executable_users)
       ])

let of_json j =
  { executable_users =
      ExecutableByStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ExecutableBy"))
  ; image_ids =
      ImageIdStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageId"))
  ; owners = OwnerStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Owner"))
  ; include_deprecated =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeDeprecated") Boolean.of_json
  ; include_disabled =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeDisabled") Boolean.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  }
