import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Articul extends App
{
  type Graph = mutable.Map[Int , List[Int]]
  val gra:mutable.Map[Int, List[Int]] = mutable.Map( 0 -> List(1,2,3), 1 -> List(0,2) , 2 ->List(0,1) , 3 -> List(0,4) , 4 ->List(3))
  val lst = List(0,1,2,3,4)
  val obj = new Articul
  println(obj.articul(gra,lst))
}

class Articul {
  type Graph = mutable.Map[Int , List[Int]]

def articul(graph:Graph,list:List[Int]):ListBuffer[Int] = {
  var temp = 0
  var output = new ListBuffer[Int]
  for(x <- list)
    {
      var m = graph.-(x)
      val adj = graph.get(x).toList.head
      for (z <- adj)
        {
          var adj1 = graph.get(z).toList.head.filter(_!=x)
          m += (z -> adj1)
        }
      
      if(x == 0) temp = 1 else temp = 0
     val res = DFS(temp,m)
      if(res.length<list.length-1)
        output += x

    }
  def DFS(start: Int, g: Graph): List[Int]={
    var visited=List(start)
    var result=List(start)

    def DFS0(start: Int): Unit={
      for(n<-g(start); if !visited.contains(n)){
        visited=n :: visited
        result=n :: result
        DFS0(n)
      }}
    DFS0(start)
    result.reverse
  }
output

}

}