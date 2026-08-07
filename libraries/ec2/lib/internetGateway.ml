open Aws.BaseTypes

type t =
  { attachments : InternetGatewayAttachmentList.t
  ; internet_gateway_id : String.t
  ; owner_id : String.t option
  ; tags : TagList.t
  }

let make ?(attachments = []) ~internet_gateway_id ?owner_id ?(tags = []) () =
  { attachments; internet_gateway_id; owner_id; tags }

let parse xml =
  Some
    { attachments =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "attachmentSet" xml)
             InternetGatewayAttachmentList.parse)
    ; internet_gateway_id =
        Aws.Xml.required
          "internetGatewayId"
          (Aws.Util.option_bind (Aws.Xml.member "internetGatewayId" xml) String.parse)
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Some
           (Aws.Query.Pair ("InternetGatewayId", String.to_query v.internet_gateway_id))
       ; Some
           (Aws.Query.Pair
              ("AttachmentSet", InternetGatewayAttachmentList.to_query v.attachments))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Some ("internetGatewayId", String.to_json v.internet_gateway_id)
       ; Some ("attachmentSet", InternetGatewayAttachmentList.to_json v.attachments)
       ])

let of_json j =
  { attachments =
      InternetGatewayAttachmentList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "attachmentSet"))
  ; internet_gateway_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "internetGatewayId"))
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
