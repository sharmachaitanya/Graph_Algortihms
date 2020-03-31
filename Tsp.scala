import scala.collection.mutable
object Tsp extends App
{
  type Graph = mutable.Map[Int , List[Int]]
  val gra:Graph = mutable.Map(0 -> List(1,2,3) , 1 -> List(0,2,3) , 2 -> List(0,1,3), 3 -> List(0,1,2))
val w = Array(Array(0,10,15,20), Array(10,0,35,25), Array(15,35,0,30), Array(20,25,30,0))
val obj = new Tsp
obj.salesman(0,w,gra , 0 , 1)
}
class Tsp {
  type Graph = mutable.Map[Int , List[Int]]
  def salesman(start:Int, weight:Array[Array[Int]], g: Graph, a:Int, b:Int):Unit =
{
  println(start + " ")
  var cost = a
  var count = b
  var min = Int.MaxValue
  var min_Index = -1
  if (count<4) {
    var adj = g.get(start).toList.head
    for (x <- adj; if adj.nonEmpty) {
      if (weight(start)(x) < min && weight(start)(x) != 0) {
        min = weight(start)(x)
        min_Index = x
      }
    }
    println(" min cost is " + min + " " + "min index is "+min_Index)
    cost = cost + min
    count = count + 1
    var m = g.-(start)
    for ((k, _) <- m) {
      val adj = g.get(k).toList.head.filter(_ != start)
      m = m + (k -> adj)
    }
    //println(m)
    println(" cost is " + cost)
    println(" count is " + count)
    salesman(min_Index, weight, m, cost, count)
  }
  else
  {
    val temp = weight(start)(0)
    cost = cost + temp
    //println(start + " " + 0 + " ")
    println("final cost is " + cost)
  }

}
}