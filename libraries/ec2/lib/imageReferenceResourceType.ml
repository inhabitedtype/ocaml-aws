open Aws.BaseTypes

type t =
  | Ec2_Instance
  | Ec2_LaunchTemplate
  | Ssm_Parameter
  | Imagebuilder_ImageRecipe
  | Imagebuilder_ContainerRecipe

let str_to_t =
  [ "imagebuilder:ContainerRecipe", Imagebuilder_ContainerRecipe
  ; "imagebuilder:ImageRecipe", Imagebuilder_ImageRecipe
  ; "ssm:Parameter", Ssm_Parameter
  ; "ec2:LaunchTemplate", Ec2_LaunchTemplate
  ; "ec2:Instance", Ec2_Instance
  ]

let t_to_str =
  [ Imagebuilder_ContainerRecipe, "imagebuilder:ContainerRecipe"
  ; Imagebuilder_ImageRecipe, "imagebuilder:ImageRecipe"
  ; Ssm_Parameter, "ssm:Parameter"
  ; Ec2_LaunchTemplate, "ec2:LaunchTemplate"
  ; Ec2_Instance, "ec2:Instance"
  ]

let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
let make v () = v

let parse xml =
  Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

let to_query v =
  Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
