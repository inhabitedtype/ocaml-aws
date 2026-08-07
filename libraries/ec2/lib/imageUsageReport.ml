open Aws.BaseTypes

type t =
  { image_id : String.t option
  ; report_id : String.t option
  ; resource_types : ImageUsageResourceTypeList.t
  ; account_ids : UserIdList.t
  ; state : String.t option
  ; state_reason : String.t option
  ; creation_time : DateTime.t option
  ; expiration_time : DateTime.t option
  ; tags : TagList.t
  }

let make
    ?image_id
    ?report_id
    ?(resource_types = [])
    ?(account_ids = [])
    ?state
    ?state_reason
    ?creation_time
    ?expiration_time
    ?(tags = [])
    () =
  { image_id
  ; report_id
  ; resource_types
  ; account_ids
  ; state
  ; state_reason
  ; creation_time
  ; expiration_time
  ; tags
  }

let parse xml =
  Some
    { image_id = Aws.Util.option_bind (Aws.Xml.member "imageId" xml) String.parse
    ; report_id = Aws.Util.option_bind (Aws.Xml.member "reportId" xml) String.parse
    ; resource_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "resourceTypeSet" xml)
             ImageUsageResourceTypeList.parse)
    ; account_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "accountIdSet" xml) UserIdList.parse)
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) String.parse
    ; state_reason = Aws.Util.option_bind (Aws.Xml.member "stateReason" xml) String.parse
    ; creation_time =
        Aws.Util.option_bind (Aws.Xml.member "creationTime" xml) DateTime.parse
    ; expiration_time =
        Aws.Util.option_bind (Aws.Xml.member "expirationTime" xml) DateTime.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.expiration_time (fun f ->
             Aws.Query.Pair ("ExpirationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.creation_time (fun f ->
             Aws.Query.Pair ("CreationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.state_reason (fun f ->
             Aws.Query.Pair ("StateReason", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", String.to_query f))
       ; Some (Aws.Query.Pair ("AccountIdSet", UserIdList.to_query v.account_ids))
       ; Some
           (Aws.Query.Pair
              ("ResourceTypeSet", ImageUsageResourceTypeList.to_query v.resource_types))
       ; Aws.Util.option_map v.report_id (fun f ->
             Aws.Query.Pair ("ReportId", String.to_query f))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.expiration_time (fun f ->
             "expirationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.creation_time (fun f -> "creationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.state_reason (fun f -> "stateReason", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", String.to_json f)
       ; Some ("accountIdSet", UserIdList.to_json v.account_ids)
       ; Some ("resourceTypeSet", ImageUsageResourceTypeList.to_json v.resource_types)
       ; Aws.Util.option_map v.report_id (fun f -> "reportId", String.to_json f)
       ; Aws.Util.option_map v.image_id (fun f -> "imageId", String.to_json f)
       ])

let of_json j =
  { image_id = Aws.Util.option_map (Aws.Json.lookup j "imageId") String.of_json
  ; report_id = Aws.Util.option_map (Aws.Json.lookup j "reportId") String.of_json
  ; resource_types =
      ImageUsageResourceTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "resourceTypeSet"))
  ; account_ids =
      UserIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "accountIdSet"))
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") String.of_json
  ; state_reason = Aws.Util.option_map (Aws.Json.lookup j "stateReason") String.of_json
  ; creation_time =
      Aws.Util.option_map (Aws.Json.lookup j "creationTime") DateTime.of_json
  ; expiration_time =
      Aws.Util.option_map (Aws.Json.lookup j "expirationTime") DateTime.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
