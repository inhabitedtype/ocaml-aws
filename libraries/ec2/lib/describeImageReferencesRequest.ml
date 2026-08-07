open Aws.BaseTypes

type t =
  { image_ids : DescribeImageReferencesImageIdStringList.t
  ; include_all_resource_types : Boolean.t option
  ; resource_types : ResourceTypeRequestList.t
  ; next_token : String.t option
  ; dry_run : Boolean.t option
  ; max_results : Integer.t option
  }

let make
    ~image_ids
    ?include_all_resource_types
    ?(resource_types = [])
    ?next_token
    ?dry_run
    ?max_results
    () =
  { image_ids
  ; include_all_resource_types
  ; resource_types
  ; next_token
  ; dry_run
  ; max_results
  }

let parse xml =
  Some
    { image_ids =
        Aws.Xml.required
          "ImageId"
          (Aws.Util.option_bind
             (Aws.Xml.member "ImageId" xml)
             DescribeImageReferencesImageIdStringList.parse)
    ; include_all_resource_types =
        Aws.Util.option_bind (Aws.Xml.member "IncludeAllResourceTypes" xml) Boolean.parse
    ; resource_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceType" xml)
             ResourceTypeRequestList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ResourceType", ResourceTypeRequestList.to_query v.resource_types))
       ; Aws.Util.option_map v.include_all_resource_types (fun f ->
             Aws.Query.Pair ("IncludeAllResourceTypes", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ImageId", DescribeImageReferencesImageIdStringList.to_query v.image_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("ResourceType", ResourceTypeRequestList.to_json v.resource_types)
       ; Aws.Util.option_map v.include_all_resource_types (fun f ->
             "IncludeAllResourceTypes", Boolean.to_json f)
       ; Some ("ImageId", DescribeImageReferencesImageIdStringList.to_json v.image_ids)
       ])

let of_json j =
  { image_ids =
      DescribeImageReferencesImageIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageId"))
  ; include_all_resource_types =
      Aws.Util.option_map (Aws.Json.lookup j "IncludeAllResourceTypes") Boolean.of_json
  ; resource_types =
      ResourceTypeRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  }
