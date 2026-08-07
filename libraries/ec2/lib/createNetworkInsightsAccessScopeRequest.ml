open Aws.BaseTypes

type t =
  { match_paths : AccessScopePathListRequest.t
  ; exclude_paths : AccessScopePathListRequest.t
  ; client_token : String.t
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  }

let make
    ?(match_paths = [])
    ?(exclude_paths = [])
    ~client_token
    ?(tag_specifications = [])
    ?dry_run
    () =
  { match_paths; exclude_paths; client_token; tag_specifications; dry_run }

let parse xml =
  Some
    { match_paths =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "MatchPath" xml)
             AccessScopePathListRequest.parse)
    ; exclude_paths =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ExcludePath" xml)
             AccessScopePathListRequest.parse)
    ; client_token =
        Aws.Xml.required
          "ClientToken"
          (Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Some (Aws.Query.Pair ("ClientToken", String.to_query v.client_token))
       ; Some
           (Aws.Query.Pair
              ("ExcludePath", AccessScopePathListRequest.to_query v.exclude_paths))
       ; Some
           (Aws.Query.Pair ("MatchPath", AccessScopePathListRequest.to_query v.match_paths))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some ("ClientToken", String.to_json v.client_token)
       ; Some ("ExcludePath", AccessScopePathListRequest.to_json v.exclude_paths)
       ; Some ("MatchPath", AccessScopePathListRequest.to_json v.match_paths)
       ])

let of_json j =
  { match_paths =
      AccessScopePathListRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "MatchPath"))
  ; exclude_paths =
      AccessScopePathListRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ExcludePath"))
  ; client_token =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ClientToken"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
