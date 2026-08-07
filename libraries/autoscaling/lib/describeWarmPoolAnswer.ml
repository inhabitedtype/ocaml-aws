open Aws.BaseTypes

type t =
  { warm_pool_configuration : WarmPoolConfiguration.t option
  ; instances : Instances.t
  ; next_token : String.t option
  }

let make ?warm_pool_configuration ?(instances = []) ?next_token () =
  { warm_pool_configuration; instances; next_token }

let parse xml =
  Some
    { warm_pool_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "WarmPoolConfiguration" xml)
          WarmPoolConfiguration.parse
    ; instances =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Instances" xml) Instances.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Instances.member", Instances.to_query v.instances))
       ; Aws.Util.option_map v.warm_pool_configuration (fun f ->
             Aws.Query.Pair ("WarmPoolConfiguration", WarmPoolConfiguration.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Instances", Instances.to_json v.instances)
       ; Aws.Util.option_map v.warm_pool_configuration (fun f ->
             "WarmPoolConfiguration", WarmPoolConfiguration.to_json f)
       ])

let of_json j =
  { warm_pool_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "WarmPoolConfiguration")
        WarmPoolConfiguration.of_json
  ; instances = Instances.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Instances"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
