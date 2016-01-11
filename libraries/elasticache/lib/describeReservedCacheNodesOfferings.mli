open Types
type input = DescribeReservedCacheNodesOfferingsMessage.t
type output = ReservedCacheNodesOfferingMessage.t
type error = Errors.t
include
  (Aws.Call with type  input :=  input and type  output :=  output and type
     error :=  error)