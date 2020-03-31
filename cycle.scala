import scala.collection.mutable



object cycle extends App
{
  //type Graph = mutable.Map[Int , List[Int]]
  val gra:mutable.Map[Int, List[Int]] = mutable.Map( 0 ->List(1,2,3), 1 ->List(0,2) , 2 ->List(0,1) , 3 -> List(0,4) , 4 ->List(3))
val gra1:mutable.Map[Int, List[Int]] = mutable.Map( 0 ->List(1), 1 ->List(0,2) , 2 ->List(1,3) , 3 -> List(2,4) , 4 ->List(3))
val obj = new cycle

val visited = new Array[Boolean](gra.size)
for(x <- visited.indices) visited(x) = false
val d = obj.isCyclic(0,gra,visited)
val e = obj.isCyclic(0,gra1,visited)
}

class cycle {
  type Graph = mutable.Map[Int , List[Int]]
  def isCyclic(start:Int,graph: Graph,visited:Array[Boolean]) =
{
  var rtrn = false
  for(x <- visited.indices ; if !visited(x))
    {
      if (isCyclicUtil(x, visited, -1)) rtrn = true else rtrn = false
    }


  def isCyclicUtil(v:Int, visited: Array[Boolean], parent:Int) : Boolean = {
    visited(v) = true
    var temp = false
    val adj = graph.get(v).toList.head
    for(i <- adj ; if adj.nonEmpty)
      if (!visited(i))
      {
        if (isCyclicUtil(i, visited, v))
          temp = true
        else if (i != parent)
          temp = true
        else temp = false
      }
    temp
  }
  rtrn
}
}