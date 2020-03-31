import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object topological extends App{
 type Graph = mutable.Map[Int , List[Int]]
  val gra:Graph = mutable.Map(0 -> List() , 1 -> List() , 2 -> List(3), 3 -> List(1) , 4 -> List(0,1) , 5 ->List(2,0) )
var VisitedArray = new Array[Boolean](gra.size)
for(x <- VisitedArray.indices)
{
  VisitedArray(x) = false
}
 val obj = new topological
 val d = obj.TopologicalSort(0,gra,VisitedArray)


}

class topological {
  type Graph = mutable.Map[Int , List[Int]]
  def TopologicalSort(start:Int , gra:Graph , visited:Array[Boolean]) = {
  var sl = ListBuffer[Int]()
  for((k, _) <- gra; if !visited(k))
  
    loop(k, visited)

   def loop(k:Int,visited1:Array[Boolean]):Unit = {

  visited1(k) = true
  val adj = gra.get(k).toList.head
  if(adj.nonEmpty) for(x <- adj){
    if(!visited1(x)) {
      loop(x, visited1)
    }
  }
 
  sl += k
}
  print(sl)
sl
}
  
}