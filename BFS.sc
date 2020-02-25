
import scala.collection.mutable._
type Graph = Map[Int , List[Int]]

val gra:Graph = Map(1 -> List(2,4),2 -> List(3,5,7,8), 3 ->List(4,2,10,9), 4 -> List(1,3) ,
  5 -> List(6,7) ,6 ->List(),7 ->List(5,8), 8 -> List(2,7),9 ->List(3),10 -> List(3))

def BFS(start:Int ,gra:Graph): Unit ={
  var visited=List(start)
  var result=List(start)

  val adj  = gra.get(start).toList.head

  for(x <- adj ; if !visited.contains(x))
  {
    visited = x :: visited
  }
  val q = visited.reverse
  loop(q)
  def loop(visited:List[Int]) :Unit = {
    for (x <- visited; if !result.contains(x)) {
      result = x :: result
      val temp = (gra.get(x).toList).head
      val temp1 = visited ++ temp
      loop(temp1)
    }
  }
  println(result.reverse)


}
BFS(1,gra)


