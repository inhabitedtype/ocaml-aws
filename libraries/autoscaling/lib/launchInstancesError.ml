open Aws.BaseTypes

type t =
  { instance_type : String.t option
  ; market_type : String.t option
  ; subnet_id : String.t option
  ; availability_zone : String.t option
  ; availability_zone_id : String.t option
  ; error_code : String.t option
  ; error_message : String.t option
  }

let make
    ?instance_type
    ?market_type
    ?subnet_id
    ?availability_zone
    ?availability_zone_id
    ?error_code
    ?error_message
    () =
  { instance_type
  ; market_type
  ; subnet_id
  ; availability_zone
  ; availability_zone_id
  ; error_code
  ; error_message
  }

let parse xml =
  Some
    { instance_type =
        Aws.Util.option_bind (Aws.Xml.member "InstanceType" xml) String.parse
    ; market_type = Aws.Util.option_bind (Aws.Xml.member "MarketType" xml) String.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "SubnetId" xml) String.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZoneId" xml) String.parse
    ; error_code = Aws.Util.option_bind (Aws.Xml.member "ErrorCode" xml) String.parse
    ; error_message =
        Aws.Util.option_bind (Aws.Xml.member "ErrorMessage" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.error_message (fun f ->
             Aws.Query.Pair ("ErrorMessage", String.to_query f))
       ; Aws.Util.option_map v.error_code (fun f ->
             Aws.Query.Pair ("ErrorCode", String.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.market_type (fun f ->
             Aws.Query.Pair ("MarketType", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.error_message (fun f -> "ErrorMessage", String.to_json f)
       ; Aws.Util.option_map v.error_code (fun f -> "ErrorCode", String.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "AvailabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "AvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "SubnetId", String.to_json f)
       ; Aws.Util.option_map v.market_type (fun f -> "MarketType", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "InstanceType", String.to_json f)
       ])

let of_json j =
  { instance_type = Aws.Util.option_map (Aws.Json.lookup j "InstanceType") String.of_json
  ; market_type = Aws.Util.option_map (Aws.Json.lookup j "MarketType") String.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "SubnetId") String.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZone") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZoneId") String.of_json
  ; error_code = Aws.Util.option_map (Aws.Json.lookup j "ErrorCode") String.of_json
  ; error_message = Aws.Util.option_map (Aws.Json.lookup j "ErrorMessage") String.of_json
  }
