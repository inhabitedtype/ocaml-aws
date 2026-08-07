open Aws.BaseTypes

type t =
  { continent_code : String.t option
  ; continent_name : String.t option
  ; country_code : String.t option
  ; country_name : String.t option
  ; subdivision_code : String.t option
  ; subdivision_name : String.t option
  }

let make
    ?continent_code
    ?continent_name
    ?country_code
    ?country_name
    ?subdivision_code
    ?subdivision_name
    () =
  { continent_code
  ; continent_name
  ; country_code
  ; country_name
  ; subdivision_code
  ; subdivision_name
  }

let parse xml =
  Some
    { continent_code =
        Aws.Util.option_bind (Aws.Xml.member "ContinentCode" xml) String.parse
    ; continent_name =
        Aws.Util.option_bind (Aws.Xml.member "ContinentName" xml) String.parse
    ; country_code = Aws.Util.option_bind (Aws.Xml.member "CountryCode" xml) String.parse
    ; country_name = Aws.Util.option_bind (Aws.Xml.member "CountryName" xml) String.parse
    ; subdivision_code =
        Aws.Util.option_bind (Aws.Xml.member "SubdivisionCode" xml) String.parse
    ; subdivision_name =
        Aws.Util.option_bind (Aws.Xml.member "SubdivisionName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subdivision_name (fun f ->
             Aws.Query.Pair ("SubdivisionName", String.to_query f))
       ; Aws.Util.option_map v.subdivision_code (fun f ->
             Aws.Query.Pair ("SubdivisionCode", String.to_query f))
       ; Aws.Util.option_map v.country_name (fun f ->
             Aws.Query.Pair ("CountryName", String.to_query f))
       ; Aws.Util.option_map v.country_code (fun f ->
             Aws.Query.Pair ("CountryCode", String.to_query f))
       ; Aws.Util.option_map v.continent_name (fun f ->
             Aws.Query.Pair ("ContinentName", String.to_query f))
       ; Aws.Util.option_map v.continent_code (fun f ->
             Aws.Query.Pair ("ContinentCode", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.subdivision_name (fun f ->
             "SubdivisionName", String.to_json f)
       ; Aws.Util.option_map v.subdivision_code (fun f ->
             "SubdivisionCode", String.to_json f)
       ; Aws.Util.option_map v.country_name (fun f -> "CountryName", String.to_json f)
       ; Aws.Util.option_map v.country_code (fun f -> "CountryCode", String.to_json f)
       ; Aws.Util.option_map v.continent_name (fun f -> "ContinentName", String.to_json f)
       ; Aws.Util.option_map v.continent_code (fun f -> "ContinentCode", String.to_json f)
       ])

let of_json j =
  { continent_code =
      Aws.Util.option_map (Aws.Json.lookup j "ContinentCode") String.of_json
  ; continent_name =
      Aws.Util.option_map (Aws.Json.lookup j "ContinentName") String.of_json
  ; country_code = Aws.Util.option_map (Aws.Json.lookup j "CountryCode") String.of_json
  ; country_name = Aws.Util.option_map (Aws.Json.lookup j "CountryName") String.of_json
  ; subdivision_code =
      Aws.Util.option_map (Aws.Json.lookup j "SubdivisionCode") String.of_json
  ; subdivision_name =
      Aws.Util.option_map (Aws.Json.lookup j "SubdivisionName") String.of_json
  }
