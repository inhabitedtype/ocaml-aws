open Aws.BaseTypes

type t =
  { keys : InsightRuleContributorKeys.t
  ; approximate_aggregate_value : Double.t
  ; datapoints : InsightRuleContributorDatapoints.t
  }

let make ~keys ~approximate_aggregate_value ~datapoints () =
  { keys; approximate_aggregate_value; datapoints }

let parse xml =
  Some
    { keys =
        Aws.Xml.required
          "Keys"
          (Aws.Util.option_bind
             (Aws.Xml.member "Keys" xml)
             InsightRuleContributorKeys.parse)
    ; approximate_aggregate_value =
        Aws.Xml.required
          "ApproximateAggregateValue"
          (Aws.Util.option_bind
             (Aws.Xml.member "ApproximateAggregateValue" xml)
             Double.parse)
    ; datapoints =
        Aws.Xml.required
          "Datapoints"
          (Aws.Util.option_bind
             (Aws.Xml.member "Datapoints" xml)
             InsightRuleContributorDatapoints.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("Datapoints.member", InsightRuleContributorDatapoints.to_query v.datapoints))
       ; Some
           (Aws.Query.Pair
              ("ApproximateAggregateValue", Double.to_query v.approximate_aggregate_value))
       ; Some (Aws.Query.Pair ("Keys.member", InsightRuleContributorKeys.to_query v.keys))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Datapoints", InsightRuleContributorDatapoints.to_json v.datapoints)
       ; Some ("ApproximateAggregateValue", Double.to_json v.approximate_aggregate_value)
       ; Some ("Keys", InsightRuleContributorKeys.to_json v.keys)
       ])

let of_json j =
  { keys =
      InsightRuleContributorKeys.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Keys"))
  ; approximate_aggregate_value =
      Double.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ApproximateAggregateValue"))
  ; datapoints =
      InsightRuleContributorDatapoints.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Datapoints"))
  }
