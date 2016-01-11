open Types
type input = DescribeAccountAttributesMessage.t
type output = AccountAttributesMessage.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)