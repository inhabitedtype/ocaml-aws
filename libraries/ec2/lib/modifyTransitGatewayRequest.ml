open Aws.BaseTypes

type t =
  { transit_gateway_id : String.t
  ; description : String.t option
  ; options : ModifyTransitGatewayOptions.t option
  ; dry_run : Boolean.t option
  }

let make ~transit_gateway_id ?description ?options ?dry_run () =
  { transit_gateway_id; description; options; dry_run }

let parse xml =
  Some
    { transit_gateway_id =
        Aws.Xml.required
          "TransitGatewayId"
          (Aws.Util.option_bind (Aws.Xml.member "TransitGatewayId" xml) String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; options =
        Aws.Util.option_bind
          (Aws.Xml.member "Options" xml)
          ModifyTransitGatewayOptions.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.options (fun f ->
             Aws.Query.Pair ("Options", ModifyTransitGatewayOptions.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some (Aws.Query.Pair ("TransitGatewayId", String.to_query v.transit_gateway_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.options (fun f ->
             "Options", ModifyTransitGatewayOptions.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("TransitGatewayId", String.to_json v.transit_gateway_id)
       ])

let of_json j =
  { transit_gateway_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitGatewayId"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; options =
      Aws.Util.option_map
        (Aws.Json.lookup j "Options")
        ModifyTransitGatewayOptions.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
