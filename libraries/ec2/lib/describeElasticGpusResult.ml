open Aws.BaseTypes

type t =
  { elastic_gpu_set : ElasticGpuSet.t
  ; max_results : Integer.t option
  ; next_token : String.t option
  }

let make ?(elastic_gpu_set = []) ?max_results ?next_token () =
  { elastic_gpu_set; max_results; next_token }

let parse xml =
  Some
    { elastic_gpu_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "elasticGpuSet" xml) ElasticGpuSet.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "maxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some (Aws.Query.Pair ("ElasticGpuSet", ElasticGpuSet.to_query v.elastic_gpu_set))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "maxResults", Integer.to_json f)
       ; Some ("elasticGpuSet", ElasticGpuSet.to_json v.elastic_gpu_set)
       ])

let of_json j =
  { elastic_gpu_set =
      ElasticGpuSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "elasticGpuSet"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "maxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
