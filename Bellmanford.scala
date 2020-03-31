import scala.collection.mutable.{ListBuffer, Map}
object Bellmanford extends App{
  type Graph = Map[Int , List[Int]]
val Edge1:Graph = Map(0 -> List(1,2,3),1-> List(4),2-> List(1,4),3-> List(2,5),4-> List(6),5-> List(6),6-> List())
val Edge:Graph = Map(0 -> List(1, 2) , 1 -> List( 2,3,4), 2-> List() , 3 -> List(1,2), 4 -> List(3) )
val w :Array[Array[Int]]= Array(Array(0,-1,4,0,0),  Array(0,0,3,2,2), Array(0,0,0,0,0), Array(0,1,5,0,0), Array(0,0,0,-3,0))
val w2 : Array[Array[Int]]= Array(Array(0,6,5,5,0,0,0),Array(0,0,0,0,-1,0,0),Array(0,-2,0,0,1,0,0),Array(0,0,-2,0,0,-1,0),Array(0,0,0,0,0,0,3),Array(0,0,0,0,0,0,3),Array(0,0,0,0,0,0,0))
val obj = new Bellmanford
val listEdge : ListBuffer[Array[Int]]= obj.edgeList(Edge)
val listEdge1 : ListBuffer[Array[Int]]= obj.edgeList(Edge1)

val d = obj.ford(0,listEdge1,w2)
for(x <- d.indices)
{
  print(d(x) + " ")
}
}

class Bellmanford {
  var N = 7
  
  def edgeList(edge:Map[Int , List[Int]]): ListBuffer[Array[Int]] =
{
  val sl = new ListBuffer[Array[Int]]
  for((k, _) <- edge )
  {
    val adj = edge.get(k).toList.head
    adj.foreach(x => sl += Array(k, x))
  }
  sl
}
  
  def ford(src:Int,lstEdge:ListBuffer[Array[Int]],weights:Array[Array[Int]]):Array[Int] = {
  val E = lstEdge.size
 
  val distance = new Array[Int](N)
  val parent = new Array[Int](N)
  for(j <- distance.indices) distance(j) = Int.MaxValue
  distance(src) = 0

  for(j <- parent.indices) parent(j) = -1



  var k = N
  while ( {{k -= 1; k} > 0})
  {
    for (j <- 0 until E) {
      var temp = lstEdge(j)
      val u = temp(0)
      val v = temp(1)


      val w = weights(u)(v)


      if (distance(u) != Int.MaxValue && (distance(u) + w < distance(v)))
      {
        distance(v) = distance(u) + w
        parent(v) = u
      }
    }
  }
  distance
}
}