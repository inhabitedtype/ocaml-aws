open Aws.BaseTypes

type t =
  { network_insights_path_id : String.t
  ; additional_accounts : ValueStringList.t
  ; filter_in_arns : ArnList.t
  ; filter_out_arns : ArnList.t
  ; dry_run : Boolean.t option
  ; tag_specifications : TagSpecificationList.t
  ; client_token : String.t
  }

let make
    ~network_insights_path_id
    ?(additional_accounts = [])
    ?(filter_in_arns = [])
    ?(filter_out_arns = [])
    ?dry_run
    ?(tag_specifications = [])
    ~client_token
    () =
  { network_insights_path_id
  ; additional_accounts
  ; filter_in_arns
  ; filter_out_arns
  ; dry_run
  ; tag_specifications
  ; client_token
  }

let parse xml =
  Some
    { network_insights_path_id =
        Aws.Xml.required
          "NetworkInsightsPathId"
          (Aws.Util.option_bind (Aws.Xml.member "NetworkInsightsPathId" xml) String.parse)
    ; additional_accounts =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AdditionalAccount" xml)
             ValueStringList.parse)
    ; filter_in_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "FilterInArn" xml) ArnList.parse)
    ; filter_out_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "FilterOutArn" xml) ArnList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; client_token =
        Aws.Xml.required
          "ClientToken"
          (Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ClientToken", String.to_query v.client_token))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("FilterOutArn", ArnList.to_query v.filter_out_arns))
       ; Some (Aws.Query.Pair ("FilterInArn", ArnList.to_query v.filter_in_arns))
       ; Some
           (Aws.Query.Pair
              ("AdditionalAccount", ValueStringList.to_query v.additional_accounts))
       ; Some
           (Aws.Query.Pair
              ("NetworkInsightsPathId", String.to_query v.network_insights_path_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ClientToken", String.to_json v.client_token)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("FilterOutArn", ArnList.to_json v.filter_out_arns)
       ; Some ("FilterInArn", ArnList.to_json v.filter_in_arns)
       ; Some ("AdditionalAccount", ValueStringList.to_json v.additional_accounts)
       ; Some ("NetworkInsightsPathId", String.to_json v.network_insights_path_id)
       ])

let of_json j =
  { network_insights_path_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NetworkInsightsPathId"))
  ; additional_accounts =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AdditionalAccount"))
  ; filter_in_arns =
      ArnList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "FilterInArn"))
  ; filter_out_arns =
      ArnList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "FilterOutArn"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; client_token =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ClientToken"))
  }
