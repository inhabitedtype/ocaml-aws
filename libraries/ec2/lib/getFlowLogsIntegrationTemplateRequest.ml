open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; flow_log_id : String.t
  ; config_delivery_s3_destination_arn : String.t
  ; integrate_services : IntegrateServices.t
  }

let make ?dry_run ~flow_log_id ~config_delivery_s3_destination_arn ~integrate_services ()
    =
  { dry_run; flow_log_id; config_delivery_s3_destination_arn; integrate_services }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; flow_log_id =
        Aws.Xml.required
          "FlowLogId"
          (Aws.Util.option_bind (Aws.Xml.member "FlowLogId" xml) String.parse)
    ; config_delivery_s3_destination_arn =
        Aws.Xml.required
          "ConfigDeliveryS3DestinationArn"
          (Aws.Util.option_bind
             (Aws.Xml.member "ConfigDeliveryS3DestinationArn" xml)
             String.parse)
    ; integrate_services =
        Aws.Xml.required
          "IntegrateService"
          (Aws.Util.option_bind
             (Aws.Xml.member "IntegrateService" xml)
             IntegrateServices.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("IntegrateService", IntegrateServices.to_query v.integrate_services))
       ; Some
           (Aws.Query.Pair
              ( "ConfigDeliveryS3DestinationArn"
              , String.to_query v.config_delivery_s3_destination_arn ))
       ; Some (Aws.Query.Pair ("FlowLogId", String.to_query v.flow_log_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("IntegrateService", IntegrateServices.to_json v.integrate_services)
       ; Some
           ( "ConfigDeliveryS3DestinationArn"
           , String.to_json v.config_delivery_s3_destination_arn )
       ; Some ("FlowLogId", String.to_json v.flow_log_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; flow_log_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "FlowLogId"))
  ; config_delivery_s3_destination_arn =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ConfigDeliveryS3DestinationArn"))
  ; integrate_services =
      IntegrateServices.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "IntegrateService"))
  }
