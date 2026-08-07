open Aws.BaseTypes

type t =
  { recurring_charge_amount : Double.t option
  ; recurring_charge_frequency : String.t option
  }

let make ?recurring_charge_amount ?recurring_charge_frequency () =
  { recurring_charge_amount; recurring_charge_frequency }

let parse xml =
  Some
    { recurring_charge_amount =
        Aws.Util.option_bind (Aws.Xml.member "RecurringChargeAmount" xml) Double.parse
    ; recurring_charge_frequency =
        Aws.Util.option_bind (Aws.Xml.member "RecurringChargeFrequency" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.recurring_charge_frequency (fun f ->
             Aws.Query.Pair ("RecurringChargeFrequency", String.to_query f))
       ; Aws.Util.option_map v.recurring_charge_amount (fun f ->
             Aws.Query.Pair ("RecurringChargeAmount", Double.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.recurring_charge_frequency (fun f ->
             "RecurringChargeFrequency", String.to_json f)
       ; Aws.Util.option_map v.recurring_charge_amount (fun f ->
             "RecurringChargeAmount", Double.to_json f)
       ])

let of_json j =
  { recurring_charge_amount =
      Aws.Util.option_map (Aws.Json.lookup j "RecurringChargeAmount") Double.of_json
  ; recurring_charge_frequency =
      Aws.Util.option_map (Aws.Json.lookup j "RecurringChargeFrequency") String.of_json
  }
