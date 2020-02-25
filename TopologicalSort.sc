import scala.collection.mutable

type Graph = mutable.Map[Int , List[Int]]

val gra:Graph = mutable.Map(0 -> List() , 1 -> List() , 2 -> List(3), 3 -> List(1) , 4 -> List(0,1) , 5 ->List(2,0) )
var VisitedArray = new Array[Boolean](gra.size)
for(x <- VisitedArray.indices)
{
  VisitedArray(x) = false
}
def TopologicalSort(start:Int , gra:Graph , ar:Array[Boolean]) = {
  var sl: List[Int] = List()
  for((k, _) <- gra; if !ar(k))
    loop(k, ar)

def loop(k:Int,ar1:Array[Boolean]):Unit = {

  ar1(k) = true
  val adj = gra.get(k).toList.head
  if(adj.nonEmpty) for(x <- adj){
    if(!ar1(x)) {
      loop(x, ar1)
    }
  }
  sl = k :: sl
}
sl
}
val d = TopologicalSort(0,gra,VisitedArray)
for(x <- d.reverse)
  {
    print(x + " ")
  }

