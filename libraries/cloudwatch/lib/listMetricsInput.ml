open Aws.BaseTypes

type t =
  { namespace : String.t option
  ; metric_name : String.t option
  ; dimensions : DimensionFilters.t
  ; next_token : String.t option
  ; recently_active : RecentlyActive.t option
  ; include_linked_accounts : Boolean.t option
  ; owning_account : String.t option
  }

let make
    ?namespace
    ?metric_name
    ?(dimensions = [])
    ?next_token
    ?recently_active
    ?include_linked_accounts
    ?owning_account
    () =
  { namespace
  ; metric_name
  ; dimensions
  ; next_token
  ; recently_active
  ; include_linked_accounts
  ; owning_account
  }

let parse xml =
  Some
    { namespace = Aws.Util.option_bind (Aws.Xml.member "Namespace" xml) String.parse
    ; metric_name = Aws.Util.option_bind (Aws.Xml.member "MetricName" xml) String.parse
    ; dimensions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Dimensions" xml) DimensionFilters.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; recently_active =
        Aws.Util.option_bind (Aws.Xml.member "RecentlyActive" xml) RecentlyActive.parse
    ; include_linked_accounts =
        Aws.Util.option_bind (Aws.Xml.member "IncludeLinkedAccounts" xml) Boolean.parse
    ; owning_account =
        Aws.Util.option_bind (Aws.Xml.member "OwningAccount" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.owning_account (fun f ->
             Aws.Query.Pair ("OwningAccount", String.to_query f))
       ; Aws.Util.option_map v.include_linked_accounts (fun f ->
             Aws.Query.Pair ("IncludeLinkedAccounts", Boolean.to_query f))
       ; Aws.Util.option_map v.recently_active (fun f ->
             Aws.Query.Pair ("RecentlyActive", RecentlyActive.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair ("Dimensions.member", DimensionFilters.to_query v.dimensions))
       ; Aws.Util.option_map v.metric_name (fun f ->
             Aws.Query.Pair ("MetricName", String.to_query f))
       ; Aws.Util.option_map v.namespace (fun f ->
             Aws.Query.Pair ("Namespace", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.owning_account (fun f -> "OwningAccount", String.to_json f)
       ; Aws.Util.option_map v.include_linked_accounts (fun f ->
             "IncludeLinkedAccounts", Boolean.to_json f)
       ; Aws.Util.option_map v.recently_active (fun f ->
             "RecentlyActive", RecentlyActive.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Dimensions", DimensionFilters.to_json v.dimensions)
       ; Aws.Util.option_map v.metric_name (fun f -> "MetricName", String.to_json f)
       ; Aws.Util.option_map v.namespace (fun f -> "Namespace", String.to_json f)
       ])

let of_json j =
  { namespace = Aws.Util.option_map (Aws.Json.lookup j "Namespace") String.of_json
  ; metric_name = Aws.Util.option_map (Aws.Json.lookup j "MetricName") String.of_json
  ; dimensions =
      DimensionFilters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Dimensions"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; recently_active =
      Aws.Util.option_map (Aws.Json.lookup j "RecentlyActive") RecentlyActive.of_json
  ; include_linked_accounts =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeLinkedAccounts") Boolean.of_json
  ; owning_account =
      Aws.Util.option_map (Aws.Json.lookup j "OwningAccount") String.of_json
  }
