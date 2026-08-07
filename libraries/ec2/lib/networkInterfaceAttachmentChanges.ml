open Aws.BaseTypes

type t =
  { default_ena_queue_count : Boolean.t option
  ; ena_queue_count : Integer.t option
  ; attachment_id : String.t option
  ; delete_on_termination : Boolean.t option
  }

let make
    ?default_ena_queue_count
    ?ena_queue_count
    ?attachment_id
    ?delete_on_termination
    () =
  { default_ena_queue_count; ena_queue_count; attachment_id; delete_on_termination }

let parse xml =
  Some
    { default_ena_queue_count =
        Aws.Util.option_bind (Aws.Xml.member "DefaultEnaQueueCount" xml) Boolean.parse
    ; ena_queue_count =
        Aws.Util.option_bind (Aws.Xml.member "EnaQueueCount" xml) Integer.parse
    ; attachment_id =
        Aws.Util.option_bind (Aws.Xml.member "attachmentId" xml) String.parse
    ; delete_on_termination =
        Aws.Util.option_bind (Aws.Xml.member "deleteOnTermination" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delete_on_termination (fun f ->
             Aws.Query.Pair ("DeleteOnTermination", Boolean.to_query f))
       ; Aws.Util.option_map v.attachment_id (fun f ->
             Aws.Query.Pair ("AttachmentId", String.to_query f))
       ; Aws.Util.option_map v.ena_queue_count (fun f ->
             Aws.Query.Pair ("EnaQueueCount", Integer.to_query f))
       ; Aws.Util.option_map v.default_ena_queue_count (fun f ->
             Aws.Query.Pair ("DefaultEnaQueueCount", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delete_on_termination (fun f ->
             "deleteOnTermination", Boolean.to_json f)
       ; Aws.Util.option_map v.attachment_id (fun f -> "attachmentId", String.to_json f)
       ; Aws.Util.option_map v.ena_queue_count (fun f ->
             "EnaQueueCount", Integer.to_json f)
       ; Aws.Util.option_map v.default_ena_queue_count (fun f ->
             "DefaultEnaQueueCount", Boolean.to_json f)
       ])

let of_json j =
  { default_ena_queue_count =
      Aws.Util.option_map (Aws.Json.lookup j "DefaultEnaQueueCount") Boolean.of_json
  ; ena_queue_count =
      Aws.Util.option_map (Aws.Json.lookup j "EnaQueueCount") Integer.of_json
  ; attachment_id = Aws.Util.option_map (Aws.Json.lookup j "attachmentId") String.of_json
  ; delete_on_termination =
      Aws.Util.option_map (Aws.Json.lookup j "deleteOnTermination") Boolean.of_json
  }
