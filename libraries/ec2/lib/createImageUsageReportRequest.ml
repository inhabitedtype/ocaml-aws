open Aws.BaseTypes

type t =
  { image_id : String.t
  ; dry_run : Boolean.t option
  ; resource_types : ImageUsageResourceTypeRequestList.t
  ; account_ids : ImageUsageReportUserIdStringList.t
  ; client_token : String.t option
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ~image_id
    ?dry_run
    ~resource_types
    ?(account_ids = [])
    ?client_token
    ?(tag_specifications = [])
    () =
  { image_id; dry_run; resource_types; account_ids; client_token; tag_specifications }

let parse xml =
  Some
    { image_id =
        Aws.Xml.required
          "ImageId"
          (Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; resource_types =
        Aws.Xml.required
          "ResourceType"
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceType" xml)
             ImageUsageResourceTypeRequestList.parse)
    ; account_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AccountId" xml)
             ImageUsageReportUserIdStringList.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AccountId", ImageUsageReportUserIdStringList.to_query v.account_ids))
       ; Some
           (Aws.Query.Pair
              ("ResourceType", ImageUsageResourceTypeRequestList.to_query v.resource_types))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("ImageId", String.to_query v.image_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("AccountId", ImageUsageReportUserIdStringList.to_json v.account_ids)
       ; Some ("ResourceType", ImageUsageResourceTypeRequestList.to_json v.resource_types)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("ImageId", String.to_json v.image_id)
       ])

let of_json j =
  { image_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ImageId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; resource_types =
      ImageUsageResourceTypeRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
  ; account_ids =
      ImageUsageReportUserIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AccountId"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
