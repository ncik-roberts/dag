include Dag.Daglike with module Vertex = Temp

val into_dag : Dag.dag * (Dag.Vertex.t -> Temp.t) * (Temp.t -> Dag.Vertex.t) -> dag
