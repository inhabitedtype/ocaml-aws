open Aws.BaseTypes

type t =
  { source_cidr_block : String.t option
  ; source_port_range : String.t option
  ; destination_cidr_block : String.t option
  ; destination_port_range : String.t option
  ; protocol : String.t option
  ; meta_data : TransitGatewayPolicyRuleMetaData.t option
  }

let make
    ?source_cidr_block
    ?source_port_range
    ?destination_cidr_block
    ?destination_port_range
    ?protocol
    ?meta_data
    () =
  { source_cidr_block
  ; source_port_range
  ; destination_cidr_block
  ; destination_port_range
  ; protocol
  ; meta_data
  }

let parse xml =
  Some
    { source_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "sourceCidrBlock" xml) String.parse
    ; source_port_range =
        Aws.Util.option_bind (Aws.Xml.member "sourcePortRange" xml) String.parse
    ; destination_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "destinationCidrBlock" xml) String.parse
    ; destination_port_range =
        Aws.Util.option_bind (Aws.Xml.member "destinationPortRange" xml) String.parse
    ; protocol = Aws.Util.option_bind (Aws.Xml.member "protocol" xml) String.parse
    ; meta_data =
        Aws.Util.option_bind
          (Aws.Xml.member "metaData" xml)
          TransitGatewayPolicyRuleMetaData.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.meta_data (fun f ->
             Aws.Query.Pair ("MetaData", TransitGatewayPolicyRuleMetaData.to_query f))
       ; Aws.Util.option_map v.protocol (fun f ->
             Aws.Query.Pair ("Protocol", String.to_query f))
       ; Aws.Util.option_map v.destination_port_range (fun f ->
             Aws.Query.Pair ("DestinationPortRange", String.to_query f))
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             Aws.Query.Pair ("DestinationCidrBlock", String.to_query f))
       ; Aws.Util.option_map v.source_port_range (fun f ->
             Aws.Query.Pair ("SourcePortRange", String.to_query f))
       ; Aws.Util.option_map v.source_cidr_block (fun f ->
             Aws.Query.Pair ("SourceCidrBlock", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.meta_data (fun f ->
             "metaData", TransitGatewayPolicyRuleMetaData.to_json f)
       ; Aws.Util.option_map v.protocol (fun f -> "protocol", String.to_json f)
       ; Aws.Util.option_map v.destination_port_range (fun f ->
             "destinationPortRange", String.to_json f)
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             "destinationCidrBlock", String.to_json f)
       ; Aws.Util.option_map v.source_port_range (fun f ->
             "sourcePortRange", String.to_json f)
       ; Aws.Util.option_map v.source_cidr_block (fun f ->
             "sourceCidrBlock", String.to_json f)
       ])

let of_json j =
  { source_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "sourceCidrBlock") String.of_json
  ; source_port_range =
      Aws.Util.option_map (Aws.Json.lookup j "sourcePortRange") String.of_json
  ; destination_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "destinationCidrBlock") String.of_json
  ; destination_port_range =
      Aws.Util.option_map (Aws.Json.lookup j "destinationPortRange") String.of_json
  ; protocol = Aws.Util.option_map (Aws.Json.lookup j "protocol") String.of_json
  ; meta_data =
      Aws.Util.option_map
        (Aws.Json.lookup j "metaData")
        TransitGatewayPolicyRuleMetaData.of_json
  }
