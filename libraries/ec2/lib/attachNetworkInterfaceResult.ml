open Aws.BaseTypes

type t =
  { attachment_id : String.t option
  ; network_card_index : Integer.t option
  }

let make ?attachment_id ?network_card_index () = { attachment_id; network_card_index }

let parse xml =
  Some
    { attachment_id =
        Aws.Util.option_bind (Aws.Xml.member "attachmentId" xml) String.parse
    ; network_card_index =
        Aws.Util.option_bind (Aws.Xml.member "networkCardIndex" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_card_index (fun f ->
             Aws.Query.Pair ("NetworkCardIndex", Integer.to_query f))
       ; Aws.Util.option_map v.attachment_id (fun f ->
             Aws.Query.Pair ("AttachmentId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_card_index (fun f ->
             "networkCardIndex", Integer.to_json f)
       ; Aws.Util.option_map v.attachment_id (fun f -> "attachmentId", String.to_json f)
       ])

let of_json j =
  { attachment_id = Aws.Util.option_map (Aws.Json.lookup j "attachmentId") String.of_json
  ; network_card_index =
      Aws.Util.option_map (Aws.Json.lookup j "networkCardIndex") Integer.of_json
  }
