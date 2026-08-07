open Aws.BaseTypes

type t =
  { geo_location_details_list : GeoLocationDetailsList.t
  ; is_truncated : Boolean.t
  ; next_continent_code : String.t option
  ; next_country_code : String.t option
  ; next_subdivision_code : String.t option
  ; max_items : String.t
  }

let make
    ~geo_location_details_list
    ~is_truncated
    ?next_continent_code
    ?next_country_code
    ?next_subdivision_code
    ~max_items
    () =
  { geo_location_details_list
  ; is_truncated
  ; next_continent_code
  ; next_country_code
  ; next_subdivision_code
  ; max_items
  }

let parse xml =
  Some
    { geo_location_details_list =
        Aws.Xml.required
          "GeoLocationDetailsList"
          (Aws.Util.option_bind
             (Aws.Xml.member "GeoLocationDetailsList" xml)
             GeoLocationDetailsList.parse)
    ; is_truncated =
        Aws.Xml.required
          "IsTruncated"
          (Aws.Util.option_bind (Aws.Xml.member "IsTruncated" xml) Boolean.parse)
    ; next_continent_code =
        Aws.Util.option_bind (Aws.Xml.member "NextContinentCode" xml) String.parse
    ; next_country_code =
        Aws.Util.option_bind (Aws.Xml.member "NextCountryCode" xml) String.parse
    ; next_subdivision_code =
        Aws.Util.option_bind (Aws.Xml.member "NextSubdivisionCode" xml) String.parse
    ; max_items =
        Aws.Xml.required
          "MaxItems"
          (Aws.Util.option_bind (Aws.Xml.member "MaxItems" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("MaxItems", String.to_query v.max_items))
       ; Aws.Util.option_map v.next_subdivision_code (fun f ->
             Aws.Query.Pair ("NextSubdivisionCode", String.to_query f))
       ; Aws.Util.option_map v.next_country_code (fun f ->
             Aws.Query.Pair ("NextCountryCode", String.to_query f))
       ; Aws.Util.option_map v.next_continent_code (fun f ->
             Aws.Query.Pair ("NextContinentCode", String.to_query f))
       ; Some (Aws.Query.Pair ("IsTruncated", Boolean.to_query v.is_truncated))
       ; Some
           (Aws.Query.Pair
              ( "GeoLocationDetailsList.member"
              , GeoLocationDetailsList.to_query v.geo_location_details_list ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("MaxItems", String.to_json v.max_items)
       ; Aws.Util.option_map v.next_subdivision_code (fun f ->
             "NextSubdivisionCode", String.to_json f)
       ; Aws.Util.option_map v.next_country_code (fun f ->
             "NextCountryCode", String.to_json f)
       ; Aws.Util.option_map v.next_continent_code (fun f ->
             "NextContinentCode", String.to_json f)
       ; Some ("IsTruncated", Boolean.to_json v.is_truncated)
       ; Some
           ( "GeoLocationDetailsList"
           , GeoLocationDetailsList.to_json v.geo_location_details_list )
       ])

let of_json j =
  { geo_location_details_list =
      GeoLocationDetailsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GeoLocationDetailsList"))
  ; is_truncated =
      Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IsTruncated"))
  ; next_continent_code =
      Aws.Util.option_map (Aws.Json.lookup j "NextContinentCode") String.of_json
  ; next_country_code =
      Aws.Util.option_map (Aws.Json.lookup j "NextCountryCode") String.of_json
  ; next_subdivision_code =
      Aws.Util.option_map (Aws.Json.lookup j "NextSubdivisionCode") String.of_json
  ; max_items = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxItems"))
  }
