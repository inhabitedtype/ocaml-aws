open Aws.BaseTypes

type t =
  { currency_code : String.t option
  ; is_valid_exchange : Boolean.t option
  ; output_reserved_instances_will_expire_at : DateTime.t option
  ; payment_due : String.t option
  ; reserved_instance_value_rollup : ReservationValue.t option
  ; reserved_instance_value_set : ReservedInstanceReservationValueSet.t
  ; target_configuration_value_rollup : ReservationValue.t option
  ; target_configuration_value_set : TargetReservationValueSet.t
  ; validation_failure_reason : String.t option
  }

let make
    ?currency_code
    ?is_valid_exchange
    ?output_reserved_instances_will_expire_at
    ?payment_due
    ?reserved_instance_value_rollup
    ?(reserved_instance_value_set = [])
    ?target_configuration_value_rollup
    ?(target_configuration_value_set = [])
    ?validation_failure_reason
    () =
  { currency_code
  ; is_valid_exchange
  ; output_reserved_instances_will_expire_at
  ; payment_due
  ; reserved_instance_value_rollup
  ; reserved_instance_value_set
  ; target_configuration_value_rollup
  ; target_configuration_value_set
  ; validation_failure_reason
  }

let parse xml =
  Some
    { currency_code =
        Aws.Util.option_bind (Aws.Xml.member "currencyCode" xml) String.parse
    ; is_valid_exchange =
        Aws.Util.option_bind (Aws.Xml.member "isValidExchange" xml) Boolean.parse
    ; output_reserved_instances_will_expire_at =
        Aws.Util.option_bind
          (Aws.Xml.member "outputReservedInstancesWillExpireAt" xml)
          DateTime.parse
    ; payment_due = Aws.Util.option_bind (Aws.Xml.member "paymentDue" xml) String.parse
    ; reserved_instance_value_rollup =
        Aws.Util.option_bind
          (Aws.Xml.member "reservedInstanceValueRollup" xml)
          ReservationValue.parse
    ; reserved_instance_value_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "reservedInstanceValueSet" xml)
             ReservedInstanceReservationValueSet.parse)
    ; target_configuration_value_rollup =
        Aws.Util.option_bind
          (Aws.Xml.member "targetConfigurationValueRollup" xml)
          ReservationValue.parse
    ; target_configuration_value_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "targetConfigurationValueSet" xml)
             TargetReservationValueSet.parse)
    ; validation_failure_reason =
        Aws.Util.option_bind (Aws.Xml.member "validationFailureReason" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.validation_failure_reason (fun f ->
             Aws.Query.Pair ("ValidationFailureReason", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TargetConfigurationValueSet"
              , TargetReservationValueSet.to_query v.target_configuration_value_set ))
       ; Aws.Util.option_map v.target_configuration_value_rollup (fun f ->
             Aws.Query.Pair ("TargetConfigurationValueRollup", ReservationValue.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ReservedInstanceValueSet"
              , ReservedInstanceReservationValueSet.to_query v.reserved_instance_value_set
              ))
       ; Aws.Util.option_map v.reserved_instance_value_rollup (fun f ->
             Aws.Query.Pair ("ReservedInstanceValueRollup", ReservationValue.to_query f))
       ; Aws.Util.option_map v.payment_due (fun f ->
             Aws.Query.Pair ("PaymentDue", String.to_query f))
       ; Aws.Util.option_map v.output_reserved_instances_will_expire_at (fun f ->
             Aws.Query.Pair ("OutputReservedInstancesWillExpireAt", DateTime.to_query f))
       ; Aws.Util.option_map v.is_valid_exchange (fun f ->
             Aws.Query.Pair ("IsValidExchange", Boolean.to_query f))
       ; Aws.Util.option_map v.currency_code (fun f ->
             Aws.Query.Pair ("CurrencyCode", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.validation_failure_reason (fun f ->
             "validationFailureReason", String.to_json f)
       ; Some
           ( "targetConfigurationValueSet"
           , TargetReservationValueSet.to_json v.target_configuration_value_set )
       ; Aws.Util.option_map v.target_configuration_value_rollup (fun f ->
             "targetConfigurationValueRollup", ReservationValue.to_json f)
       ; Some
           ( "reservedInstanceValueSet"
           , ReservedInstanceReservationValueSet.to_json v.reserved_instance_value_set )
       ; Aws.Util.option_map v.reserved_instance_value_rollup (fun f ->
             "reservedInstanceValueRollup", ReservationValue.to_json f)
       ; Aws.Util.option_map v.payment_due (fun f -> "paymentDue", String.to_json f)
       ; Aws.Util.option_map v.output_reserved_instances_will_expire_at (fun f ->
             "outputReservedInstancesWillExpireAt", DateTime.to_json f)
       ; Aws.Util.option_map v.is_valid_exchange (fun f ->
             "isValidExchange", Boolean.to_json f)
       ; Aws.Util.option_map v.currency_code (fun f -> "currencyCode", String.to_json f)
       ])

let of_json j =
  { currency_code = Aws.Util.option_map (Aws.Json.lookup j "currencyCode") String.of_json
  ; is_valid_exchange =
      Aws.Util.option_map (Aws.Json.lookup j "isValidExchange") Boolean.of_json
  ; output_reserved_instances_will_expire_at =
      Aws.Util.option_map
        (Aws.Json.lookup j "outputReservedInstancesWillExpireAt")
        DateTime.of_json
  ; payment_due = Aws.Util.option_map (Aws.Json.lookup j "paymentDue") String.of_json
  ; reserved_instance_value_rollup =
      Aws.Util.option_map
        (Aws.Json.lookup j "reservedInstanceValueRollup")
        ReservationValue.of_json
  ; reserved_instance_value_set =
      ReservedInstanceReservationValueSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "reservedInstanceValueSet"))
  ; target_configuration_value_rollup =
      Aws.Util.option_map
        (Aws.Json.lookup j "targetConfigurationValueRollup")
        ReservationValue.of_json
  ; target_configuration_value_set =
      TargetReservationValueSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "targetConfigurationValueSet"))
  ; validation_failure_reason =
      Aws.Util.option_map (Aws.Json.lookup j "validationFailureReason") String.of_json
  }
