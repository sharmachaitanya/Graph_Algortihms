import scala.collection.mutable.Map
type Graph = Map[Int , List[Int]]
val Edge:Graph = Map(0 -> List(1, 2) , 1 -> List(1, 2, 4), 2-> List(1,2,3) , 3 -> List(2,6),4 -> List(1,5,6,7) , 5 -> List(4,7) ,6-> List(5, 4))
val x = Array(
  Array(0,10,80,0,0,0,0), Array(10,0,6,0,20,0,0), Array(0,6,0,70,0,0,0), Array(0,0,60,0,0,0,60), Array(0,0,0,0,0,50,10), Array(0,0,0,0,0,0,0), Array(0,0,0,0,0,5,0)
)
def dij (source:Int, edge:Graph, weights:Array[Array[Int]]): Unit =
{
  def printsol(dist: Array[Int], n: Int): Unit = {
    println("Vertex Distance from Source")
    for (i <- 0 until 9) println(i, dist(i))
  }
  val visited = new Array[Boolean](9)
  val distance = new Array[Int](9)
  for(i <- 0 until 9)
    {
      visited(i) = false
      distance(i) = java.lang.Integer.MAX_VALUE
    }
  distance(source) = 0
  def min(dist: Array[Int], visit: Array[Boolean]): Int = {
    var min: Int = java.lang.Integer.MAX_VALUE
    var index_min: Int = -1
    for (i <- 0 until 9) {
      if (visited(i) == false && distance(i) <= min) {
        min = distance(i)
        index_min = i
        print(i)
      }
    }
    index_min
  }

  for(i <- 0 until 9)
    {
      var u:Int = min(distance,visited)
      visited(u) = true
      var adj = edge.get(u).toList.head
      for(v <- adj)
        {
          if(visited(v) == false && distance(u) != java.lang.Integer.MAX_VALUE && distance(v) > distance(u) + weights(u)(v))
            distance(v) = distance(u) + weights(u)(v)
        }
      printsol(distance , 9)
    }}
dij(0,Edge,x)