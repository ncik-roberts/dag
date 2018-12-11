val trans : (Air.t * Annotated_air.result) list
  -> Air.t -> Tc.struct_type Tc.IdentMap.t -> Annotated_air.result -> Cuda_ir.t option
