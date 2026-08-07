open Aws.BaseTypes

type t =
  { id : String.t
  ; name : String.t
  ; caller_reference : String.t
  ; config : HostedZoneConfig.t option
  ; resource_record_set_count : Long.t option
  ; linked_service : LinkedService.t option
  ; features : HostedZoneFeatures.t option
  }

let make
    ~id
    ~name
    ~caller_reference
    ?config
    ?resource_record_set_count
    ?linked_service
    ?features
    () =
  { id
  ; name
  ; caller_reference
  ; config
  ; resource_record_set_count
  ; linked_service
  ; features
  }

let parse xml =
  Some
    { id =
        Aws.Xml.required
          "Id"
          (Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse)
    ; name =
        Aws.Xml.required
          "Name"
          (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
    ; caller_reference =
        Aws.Xml.required
          "CallerReference"
          (Aws.Util.option_bind (Aws.Xml.member "CallerReference" xml) String.parse)
    ; config = Aws.Util.option_bind (Aws.Xml.member "Config" xml) HostedZoneConfig.parse
    ; resource_record_set_count =
        Aws.Util.option_bind (Aws.Xml.member "ResourceRecordSetCount" xml) Long.parse
    ; linked_service =
        Aws.Util.option_bind (Aws.Xml.member "LinkedService" xml) LinkedService.parse
    ; features =
        Aws.Util.option_bind (Aws.Xml.member "Features" xml) HostedZoneFeatures.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.features (fun f ->
             Aws.Query.Pair ("Features", HostedZoneFeatures.to_query f))
       ; Aws.Util.option_map v.linked_service (fun f ->
             Aws.Query.Pair ("LinkedService", LinkedService.to_query f))
       ; Aws.Util.option_map v.resource_record_set_count (fun f ->
             Aws.Query.Pair ("ResourceRecordSetCount", Long.to_query f))
       ; Aws.Util.option_map v.config (fun f ->
             Aws.Query.Pair ("Config", HostedZoneConfig.to_query f))
       ; Some (Aws.Query.Pair ("CallerReference", String.to_query v.caller_reference))
       ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
       ; Some (Aws.Query.Pair ("Id", String.to_query v.id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.features (fun f ->
             "Features", HostedZoneFeatures.to_json f)
       ; Aws.Util.option_map v.linked_service (fun f ->
             "LinkedService", LinkedService.to_json f)
       ; Aws.Util.option_map v.resource_record_set_count (fun f ->
             "ResourceRecordSetCount", Long.to_json f)
       ; Aws.Util.option_map v.config (fun f -> "Config", HostedZoneConfig.to_json f)
       ; Some ("CallerReference", String.to_json v.caller_reference)
       ; Some ("Name", String.to_json v.name)
       ; Some ("Id", String.to_json v.id)
       ])

let of_json j =
  { id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Id"))
  ; name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
  ; caller_reference =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CallerReference"))
  ; config = Aws.Util.option_map (Aws.Json.lookup j "Config") HostedZoneConfig.of_json
  ; resource_record_set_count =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceRecordSetCount") Long.of_json
  ; linked_service =
      Aws.Util.option_map (Aws.Json.lookup j "LinkedService") LinkedService.of_json
  ; features =
      Aws.Util.option_map (Aws.Json.lookup j "Features") HostedZoneFeatures.of_json
  }
