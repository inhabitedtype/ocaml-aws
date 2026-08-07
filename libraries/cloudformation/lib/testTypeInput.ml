open Aws.BaseTypes

type t =
  { arn : String.t option
  ; type_ : ThirdPartyType.t option
  ; type_name : String.t option
  ; version_id : String.t option
  ; log_delivery_bucket : String.t option
  }

let make ?arn ?type_ ?type_name ?version_id ?log_delivery_bucket () =
  { arn; type_; type_name; version_id; log_delivery_bucket }

let parse xml =
  Some
    { arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) ThirdPartyType.parse
    ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
    ; version_id = Aws.Util.option_bind (Aws.Xml.member "VersionId" xml) String.parse
    ; log_delivery_bucket =
        Aws.Util.option_bind (Aws.Xml.member "LogDeliveryBucket" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.log_delivery_bucket (fun f ->
             Aws.Query.Pair ("LogDeliveryBucket", String.to_query f))
       ; Aws.Util.option_map v.version_id (fun f ->
             Aws.Query.Pair ("VersionId", String.to_query f))
       ; Aws.Util.option_map v.type_name (fun f ->
             Aws.Query.Pair ("TypeName", String.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", ThirdPartyType.to_query f))
       ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.log_delivery_bucket (fun f ->
             "LogDeliveryBucket", String.to_json f)
       ; Aws.Util.option_map v.version_id (fun f -> "VersionId", String.to_json f)
       ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", ThirdPartyType.to_json f)
       ; Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
       ])

let of_json j =
  { arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") ThirdPartyType.of_json
  ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
  ; version_id = Aws.Util.option_map (Aws.Json.lookup j "VersionId") String.of_json
  ; log_delivery_bucket =
      Aws.Util.option_map (Aws.Json.lookup j "LogDeliveryBucket") String.of_json
  }
