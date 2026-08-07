open Aws.BaseTypes

type t =
  { availability_zone : String.t option
  ; availability_zone_id : String.t option
  ; instance_type : InstanceType.t option
  ; product_description : RIProductDescription.t option
  ; spot_price : String.t option
  ; timestamp : DateTime.t option
  }

let make
    ?availability_zone
    ?availability_zone_id
    ?instance_type
    ?product_description
    ?spot_price
    ?timestamp
    () =
  { availability_zone
  ; availability_zone_id
  ; instance_type
  ; product_description
  ; spot_price
  ; timestamp
  }

let parse xml =
  Some
    { availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) InstanceType.parse
    ; product_description =
        Aws.Util.option_bind
          (Aws.Xml.member "productDescription" xml)
          RIProductDescription.parse
    ; spot_price = Aws.Util.option_bind (Aws.Xml.member "spotPrice" xml) String.parse
    ; timestamp = Aws.Util.option_bind (Aws.Xml.member "timestamp" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timestamp (fun f ->
             Aws.Query.Pair ("Timestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.spot_price (fun f ->
             Aws.Query.Pair ("SpotPrice", String.to_query f))
       ; Aws.Util.option_map v.product_description (fun f ->
             Aws.Query.Pair ("ProductDescription", RIProductDescription.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", InstanceType.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timestamp (fun f -> "timestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.spot_price (fun f -> "spotPrice", String.to_json f)
       ; Aws.Util.option_map v.product_description (fun f ->
             "productDescription", RIProductDescription.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "instanceType", InstanceType.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ])

let of_json j =
  { availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "instanceType") InstanceType.of_json
  ; product_description =
      Aws.Util.option_map
        (Aws.Json.lookup j "productDescription")
        RIProductDescription.of_json
  ; spot_price = Aws.Util.option_map (Aws.Json.lookup j "spotPrice") String.of_json
  ; timestamp = Aws.Util.option_map (Aws.Json.lookup j "timestamp") DateTime.of_json
  }
