open Aws.BaseTypes

type t =
  { enabled : Boolean.t option
  ; delivery_status : VerifiedAccessLogDeliveryStatus.t option
  ; bucket_name : String.t option
  ; prefix : String.t option
  ; bucket_owner : String.t option
  }

let make ?enabled ?delivery_status ?bucket_name ?prefix ?bucket_owner () =
  { enabled; delivery_status; bucket_name; prefix; bucket_owner }

let parse xml =
  Some
    { enabled = Aws.Util.option_bind (Aws.Xml.member "enabled" xml) Boolean.parse
    ; delivery_status =
        Aws.Util.option_bind
          (Aws.Xml.member "deliveryStatus" xml)
          VerifiedAccessLogDeliveryStatus.parse
    ; bucket_name = Aws.Util.option_bind (Aws.Xml.member "bucketName" xml) String.parse
    ; prefix = Aws.Util.option_bind (Aws.Xml.member "prefix" xml) String.parse
    ; bucket_owner = Aws.Util.option_bind (Aws.Xml.member "bucketOwner" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bucket_owner (fun f ->
             Aws.Query.Pair ("BucketOwner", String.to_query f))
       ; Aws.Util.option_map v.prefix (fun f ->
             Aws.Query.Pair ("Prefix", String.to_query f))
       ; Aws.Util.option_map v.bucket_name (fun f ->
             Aws.Query.Pair ("BucketName", String.to_query f))
       ; Aws.Util.option_map v.delivery_status (fun f ->
             Aws.Query.Pair ("DeliveryStatus", VerifiedAccessLogDeliveryStatus.to_query f))
       ; Aws.Util.option_map v.enabled (fun f ->
             Aws.Query.Pair ("Enabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bucket_owner (fun f -> "bucketOwner", String.to_json f)
       ; Aws.Util.option_map v.prefix (fun f -> "prefix", String.to_json f)
       ; Aws.Util.option_map v.bucket_name (fun f -> "bucketName", String.to_json f)
       ; Aws.Util.option_map v.delivery_status (fun f ->
             "deliveryStatus", VerifiedAccessLogDeliveryStatus.to_json f)
       ; Aws.Util.option_map v.enabled (fun f -> "enabled", Boolean.to_json f)
       ])

let of_json j =
  { enabled = Aws.Util.option_map (Aws.Json.lookup j "enabled") Boolean.of_json
  ; delivery_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "deliveryStatus")
        VerifiedAccessLogDeliveryStatus.of_json
  ; bucket_name = Aws.Util.option_map (Aws.Json.lookup j "bucketName") String.of_json
  ; prefix = Aws.Util.option_map (Aws.Json.lookup j "prefix") String.of_json
  ; bucket_owner = Aws.Util.option_map (Aws.Json.lookup j "bucketOwner") String.of_json
  }
