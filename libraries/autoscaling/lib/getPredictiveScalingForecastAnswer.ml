open Aws.BaseTypes

type t =
  { load_forecast : LoadForecasts.t
  ; capacity_forecast : CapacityForecast.t
  ; update_time : DateTime.t
  }

let make ~load_forecast ~capacity_forecast ~update_time () =
  { load_forecast; capacity_forecast; update_time }

let parse xml =
  Some
    { load_forecast =
        Aws.Xml.required
          "LoadForecast"
          (Aws.Util.option_bind (Aws.Xml.member "LoadForecast" xml) LoadForecasts.parse)
    ; capacity_forecast =
        Aws.Xml.required
          "CapacityForecast"
          (Aws.Util.option_bind
             (Aws.Xml.member "CapacityForecast" xml)
             CapacityForecast.parse)
    ; update_time =
        Aws.Xml.required
          "UpdateTime"
          (Aws.Util.option_bind (Aws.Xml.member "UpdateTime" xml) DateTime.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("UpdateTime", DateTime.to_query v.update_time))
       ; Some
           (Aws.Query.Pair
              ("CapacityForecast", CapacityForecast.to_query v.capacity_forecast))
       ; Some
           (Aws.Query.Pair ("LoadForecast.member", LoadForecasts.to_query v.load_forecast))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("UpdateTime", DateTime.to_json v.update_time)
       ; Some ("CapacityForecast", CapacityForecast.to_json v.capacity_forecast)
       ; Some ("LoadForecast", LoadForecasts.to_json v.load_forecast)
       ])

let of_json j =
  { load_forecast =
      LoadForecasts.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadForecast"))
  ; capacity_forecast =
      CapacityForecast.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CapacityForecast"))
  ; update_time =
      DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UpdateTime"))
  }
