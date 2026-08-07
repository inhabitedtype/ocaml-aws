open Aws.BaseTypes

type t =
  { verified_access_group_id : String.t option
  ; verified_access_instance_id : String.t option
  ; description : String.t option
  ; owner : String.t option
  ; verified_access_group_arn : String.t option
  ; creation_time : String.t option
  ; last_updated_time : String.t option
  ; deletion_time : String.t option
  ; tags : TagList.t
  ; sse_specification : VerifiedAccessSseSpecificationResponse.t option
  }

let make
    ?verified_access_group_id
    ?verified_access_instance_id
    ?description
    ?owner
    ?verified_access_group_arn
    ?creation_time
    ?last_updated_time
    ?deletion_time
    ?(tags = [])
    ?sse_specification
    () =
  { verified_access_group_id
  ; verified_access_instance_id
  ; description
  ; owner
  ; verified_access_group_arn
  ; creation_time
  ; last_updated_time
  ; deletion_time
  ; tags
  ; sse_specification
  }

let parse xml =
  Some
    { verified_access_group_id =
        Aws.Util.option_bind (Aws.Xml.member "verifiedAccessGroupId" xml) String.parse
    ; verified_access_instance_id =
        Aws.Util.option_bind (Aws.Xml.member "verifiedAccessInstanceId" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; owner = Aws.Util.option_bind (Aws.Xml.member "owner" xml) String.parse
    ; verified_access_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "verifiedAccessGroupArn" xml) String.parse
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "creationTime" xml) String.parse
    ; last_updated_time =
        Aws.Util.option_bind (Aws.Xml.member "lastUpdatedTime" xml) String.parse
    ; deletion_time =
        Aws.Util.option_bind (Aws.Xml.member "deletionTime" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; sse_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "sseSpecification" xml)
          VerifiedAccessSseSpecificationResponse.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.sse_specification (fun f ->
             Aws.Query.Pair
               ("SseSpecification", VerifiedAccessSseSpecificationResponse.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.deletion_time (fun f ->
             Aws.Query.Pair ("DeletionTime", String.to_query f))
       ; Aws.Util.option_map v.last_updated_time (fun f ->
             Aws.Query.Pair ("LastUpdatedTime", String.to_query f))
       ; Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", String.to_query f))
       ; Aws.Util.option_map v.verified_access_group_arn (fun f ->
             Aws.Query.Pair ("VerifiedAccessGroupArn", String.to_query f))
       ; Aws.Util.option_map v.owner (fun f ->
             Aws.Query.Pair ("Owner", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.verified_access_instance_id (fun f ->
             Aws.Query.Pair ("VerifiedAccessInstanceId", String.to_query f))
       ; Aws.Util.option_map v.verified_access_group_id (fun f ->
             Aws.Query.Pair ("VerifiedAccessGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.sse_specification (fun f ->
             "sseSpecification", VerifiedAccessSseSpecificationResponse.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.deletion_time (fun f -> "deletionTime", String.to_json f)
       ; Aws.Util.option_map v.last_updated_time (fun f ->
             "lastUpdatedTime", String.to_json f)
       ; Aws.Util.option_map v.creation_time (fun f -> "creationTime", String.to_json f)
       ; Aws.Util.option_map v.verified_access_group_arn (fun f ->
             "verifiedAccessGroupArn", String.to_json f)
       ; Aws.Util.option_map v.owner (fun f -> "owner", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.verified_access_instance_id (fun f ->
             "verifiedAccessInstanceId", String.to_json f)
       ; Aws.Util.option_map v.verified_access_group_id (fun f ->
             "verifiedAccessGroupId", String.to_json f)
       ])

let of_json j =
  { verified_access_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "verifiedAccessGroupId") String.of_json
  ; verified_access_instance_id =
      Aws.Util.option_map (Aws.Json.lookup j "verifiedAccessInstanceId") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; owner = Aws.Util.option_map (Aws.Json.lookup j "owner") String.of_json
  ; verified_access_group_arn =
      Aws.Util.option_map (Aws.Json.lookup j "verifiedAccessGroupArn") String.of_json
  ; creation_time = Aws.Util.option_map (Aws.Json.lookup j "creationTime") String.of_json
  ; last_updated_time =
      Aws.Util.option_map (Aws.Json.lookup j "lastUpdatedTime") String.of_json
  ; deletion_time = Aws.Util.option_map (Aws.Json.lookup j "deletionTime") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; sse_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "sseSpecification")
        VerifiedAccessSseSpecificationResponse.of_json
  }
