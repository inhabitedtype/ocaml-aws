open Aws.BaseTypes

type t =
  { transit_gateway_metering_policy_id : String.t
  ; add_middlebox_attachment_ids : TransitGatewayAttachmentIdStringList.t
  ; remove_middlebox_attachment_ids : TransitGatewayAttachmentIdStringList.t
  ; dry_run : Boolean.t option
  }

let make
    ~transit_gateway_metering_policy_id
    ?(add_middlebox_attachment_ids = [])
    ?(remove_middlebox_attachment_ids = [])
    ?dry_run
    () =
  { transit_gateway_metering_policy_id
  ; add_middlebox_attachment_ids
  ; remove_middlebox_attachment_ids
  ; dry_run
  }

let parse xml =
  Some
    { transit_gateway_metering_policy_id =
        Aws.Xml.required
          "TransitGatewayMeteringPolicyId"
          (Aws.Util.option_bind
             (Aws.Xml.member "TransitGatewayMeteringPolicyId" xml)
             String.parse)
    ; add_middlebox_attachment_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AddMiddleboxAttachmentId" xml)
             TransitGatewayAttachmentIdStringList.parse)
    ; remove_middlebox_attachment_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveMiddleboxAttachmentId" xml)
             TransitGatewayAttachmentIdStringList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "RemoveMiddleboxAttachmentId"
              , TransitGatewayAttachmentIdStringList.to_query
                  v.remove_middlebox_attachment_ids ))
       ; Some
           (Aws.Query.Pair
              ( "AddMiddleboxAttachmentId"
              , TransitGatewayAttachmentIdStringList.to_query
                  v.add_middlebox_attachment_ids ))
       ; Some
           (Aws.Query.Pair
              ( "TransitGatewayMeteringPolicyId"
              , String.to_query v.transit_gateway_metering_policy_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some
           ( "RemoveMiddleboxAttachmentId"
           , TransitGatewayAttachmentIdStringList.to_json
               v.remove_middlebox_attachment_ids )
       ; Some
           ( "AddMiddleboxAttachmentId"
           , TransitGatewayAttachmentIdStringList.to_json v.add_middlebox_attachment_ids
           )
       ; Some
           ( "TransitGatewayMeteringPolicyId"
           , String.to_json v.transit_gateway_metering_policy_id )
       ])

let of_json j =
  { transit_gateway_metering_policy_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitGatewayMeteringPolicyId"))
  ; add_middlebox_attachment_ids =
      TransitGatewayAttachmentIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AddMiddleboxAttachmentId"))
  ; remove_middlebox_attachment_ids =
      TransitGatewayAttachmentIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveMiddleboxAttachmentId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
