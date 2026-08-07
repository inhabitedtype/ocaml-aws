open Aws.BaseTypes

type t =
  { apply_to_all_regions : Boolean.t option
  ; advanced_event_selectors : AdvancedEventSelectors.t
  }

let make ?apply_to_all_regions ?(advanced_event_selectors = []) () =
  { apply_to_all_regions; advanced_event_selectors }

let parse xml =
  Some
    { apply_to_all_regions =
        Aws.Util.option_bind (Aws.Xml.member "ApplyToAllRegions" xml) Boolean.parse
    ; advanced_event_selectors =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AdvancedEventSelectors" xml)
             AdvancedEventSelectors.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "AdvancedEventSelectors.member"
              , AdvancedEventSelectors.to_query v.advanced_event_selectors ))
       ; Aws.Util.option_map v.apply_to_all_regions (fun f ->
             Aws.Query.Pair ("ApplyToAllRegions", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "AdvancedEventSelectors"
           , AdvancedEventSelectors.to_json v.advanced_event_selectors )
       ; Aws.Util.option_map v.apply_to_all_regions (fun f ->
             "ApplyToAllRegions", Boolean.to_json f)
       ])

let of_json j =
  { apply_to_all_regions =
      Aws.Util.option_map (Aws.Json.lookup j "ApplyToAllRegions") Boolean.of_json
  ; advanced_event_selectors =
      AdvancedEventSelectors.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AdvancedEventSelectors"))
  }
