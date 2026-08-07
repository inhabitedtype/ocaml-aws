open Aws.BaseTypes

type t =
  { attachments : InternetGatewayAttachmentList.t
  ; egress_only_internet_gateway_id : String.t option
  ; tags : TagList.t
  }

let make ?(attachments = []) ?egress_only_internet_gateway_id ?(tags = []) () =
  { attachments; egress_only_internet_gateway_id; tags }

let parse xml =
  Some
    { attachments =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "attachmentSet" xml)
             InternetGatewayAttachmentList.parse)
    ; egress_only_internet_gateway_id =
        Aws.Util.option_bind
          (Aws.Xml.member "egressOnlyInternetGatewayId" xml)
          String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.egress_only_internet_gateway_id (fun f ->
             Aws.Query.Pair ("EgressOnlyInternetGatewayId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AttachmentSet", InternetGatewayAttachmentList.to_query v.attachments))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.egress_only_internet_gateway_id (fun f ->
             "egressOnlyInternetGatewayId", String.to_json f)
       ; Some ("attachmentSet", InternetGatewayAttachmentList.to_json v.attachments)
       ])

let of_json j =
  { attachments =
      InternetGatewayAttachmentList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "attachmentSet"))
  ; egress_only_internet_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "egressOnlyInternetGatewayId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
