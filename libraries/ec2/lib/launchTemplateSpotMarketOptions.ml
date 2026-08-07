open Aws.BaseTypes

type t =
  { max_price : String.t option
  ; spot_instance_type : SpotInstanceType.t option
  ; block_duration_minutes : Integer.t option
  ; valid_until : DateTime.t option
  ; instance_interruption_behavior : InstanceInterruptionBehavior.t option
  }

let make
    ?max_price
    ?spot_instance_type
    ?block_duration_minutes
    ?valid_until
    ?instance_interruption_behavior
    () =
  { max_price
  ; spot_instance_type
  ; block_duration_minutes
  ; valid_until
  ; instance_interruption_behavior
  }

let parse xml =
  Some
    { max_price = Aws.Util.option_bind (Aws.Xml.member "maxPrice" xml) String.parse
    ; spot_instance_type =
        Aws.Util.option_bind
          (Aws.Xml.member "spotInstanceType" xml)
          SpotInstanceType.parse
    ; block_duration_minutes =
        Aws.Util.option_bind (Aws.Xml.member "blockDurationMinutes" xml) Integer.parse
    ; valid_until = Aws.Util.option_bind (Aws.Xml.member "validUntil" xml) DateTime.parse
    ; instance_interruption_behavior =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceInterruptionBehavior" xml)
          InstanceInterruptionBehavior.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_interruption_behavior (fun f ->
             Aws.Query.Pair
               ("InstanceInterruptionBehavior", InstanceInterruptionBehavior.to_query f))
       ; Aws.Util.option_map v.valid_until (fun f ->
             Aws.Query.Pair ("ValidUntil", DateTime.to_query f))
       ; Aws.Util.option_map v.block_duration_minutes (fun f ->
             Aws.Query.Pair ("BlockDurationMinutes", Integer.to_query f))
       ; Aws.Util.option_map v.spot_instance_type (fun f ->
             Aws.Query.Pair ("SpotInstanceType", SpotInstanceType.to_query f))
       ; Aws.Util.option_map v.max_price (fun f ->
             Aws.Query.Pair ("MaxPrice", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_interruption_behavior (fun f ->
             "instanceInterruptionBehavior", InstanceInterruptionBehavior.to_json f)
       ; Aws.Util.option_map v.valid_until (fun f -> "validUntil", DateTime.to_json f)
       ; Aws.Util.option_map v.block_duration_minutes (fun f ->
             "blockDurationMinutes", Integer.to_json f)
       ; Aws.Util.option_map v.spot_instance_type (fun f ->
             "spotInstanceType", SpotInstanceType.to_json f)
       ; Aws.Util.option_map v.max_price (fun f -> "maxPrice", String.to_json f)
       ])

let of_json j =
  { max_price = Aws.Util.option_map (Aws.Json.lookup j "maxPrice") String.of_json
  ; spot_instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "spotInstanceType") SpotInstanceType.of_json
  ; block_duration_minutes =
      Aws.Util.option_map (Aws.Json.lookup j "blockDurationMinutes") Integer.of_json
  ; valid_until = Aws.Util.option_map (Aws.Json.lookup j "validUntil") DateTime.of_json
  ; instance_interruption_behavior =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceInterruptionBehavior")
        InstanceInterruptionBehavior.of_json
  }
