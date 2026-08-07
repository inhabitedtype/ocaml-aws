open Aws.BaseTypes

type t =
  { verified_access_instance_id : String.t
  ; description : String.t option
  ; policy_document : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; client_token : String.t option
  ; dry_run : Boolean.t option
  ; sse_specification : VerifiedAccessSseSpecificationRequest.t option
  }

let make
    ~verified_access_instance_id
    ?description
    ?policy_document
    ?(tag_specifications = [])
    ?client_token
    ?dry_run
    ?sse_specification
    () =
  { verified_access_instance_id
  ; description
  ; policy_document
  ; tag_specifications
  ; client_token
  ; dry_run
  ; sse_specification
  }

let parse xml =
  Some
    { verified_access_instance_id =
        Aws.Xml.required
          "VerifiedAccessInstanceId"
          (Aws.Util.option_bind
             (Aws.Xml.member "VerifiedAccessInstanceId" xml)
             String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; policy_document =
        Aws.Util.option_bind (Aws.Xml.member "PolicyDocument" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; sse_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "SseSpecification" xml)
          VerifiedAccessSseSpecificationRequest.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.sse_specification (fun f ->
             Aws.Query.Pair
               ("SseSpecification", VerifiedAccessSseSpecificationRequest.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.policy_document (fun f ->
             Aws.Query.Pair ("PolicyDocument", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("VerifiedAccessInstanceId", String.to_query v.verified_access_instance_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.sse_specification (fun f ->
             "SseSpecification", VerifiedAccessSseSpecificationRequest.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.policy_document (fun f ->
             "PolicyDocument", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("VerifiedAccessInstanceId", String.to_json v.verified_access_instance_id)
       ])

let of_json j =
  { verified_access_instance_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VerifiedAccessInstanceId"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; policy_document =
      Aws.Util.option_map (Aws.Json.lookup j "PolicyDocument") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; sse_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "SseSpecification")
        VerifiedAccessSseSpecificationRequest.of_json
  }
