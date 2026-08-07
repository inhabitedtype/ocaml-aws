open Aws.BaseTypes

type t =
  { amount : Double.t option
  ; frequency : RecurringChargeFrequency.t option
  }

let make ?amount ?frequency () = { amount; frequency }

let parse xml =
  Some
    { amount = Aws.Util.option_bind (Aws.Xml.member "amount" xml) Double.parse
    ; frequency =
        Aws.Util.option_bind
          (Aws.Xml.member "frequency" xml)
          RecurringChargeFrequency.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.frequency (fun f ->
             Aws.Query.Pair ("Frequency", RecurringChargeFrequency.to_query f))
       ; Aws.Util.option_map v.amount (fun f ->
             Aws.Query.Pair ("Amount", Double.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.frequency (fun f ->
             "frequency", RecurringChargeFrequency.to_json f)
       ; Aws.Util.option_map v.amount (fun f -> "amount", Double.to_json f)
       ])

let of_json j =
  { amount = Aws.Util.option_map (Aws.Json.lookup j "amount") Double.of_json
  ; frequency =
      Aws.Util.option_map (Aws.Json.lookup j "frequency") RecurringChargeFrequency.of_json
  }
