import scala.collection.mutable.ListBuffer
object Kruskal extends App {
  val lst = new ListBuffer[Array[Int]]
  val obj = new Kruskal
  obj.addEdges(0,1,28,lst)
  obj.addEdges(1,2,16,lst)
  obj.addEdges(2,3,12,lst)
  obj.addEdges(3,4,22,lst)
  obj.addEdges(4,5,25,lst)
  obj.addEdges(0,5,10,lst)
  obj.addEdges(1,6,14,lst)
  obj.addEdges(4,6,24,lst)
  
  val lIst = lst.sortBy(_(2))
  val disset = new Array[Int](7)
  for(x <- 0 until 7)
  {
    disset(x) = x
  }
  obj.kruskal(lIst,disset)

}
class Kruskal {
  def addEdges(start:Int,dest:Int,weight:Int,list:ListBuffer[Array[Int]]):ListBuffer[Array[Int]]={

  val temp = new Array[Int](3)
  temp(0) = start
  temp(1) = dest
  temp(2) = weight
  list += temp
  }
  
  def kruskal(edges:ListBuffer[Array[Int]],disjointSet:Array[Int]): ListBuffer[Array[Int]] = {
  val outList = new ListBuffer[Array[Int]]
  for(x <- edges)
  {
    if(disjointSet(x(0))!= disjointSet(x(1)) && outList.length<disjointSet.length - 1)
    {
      unionSet(disjointSet,x(0),x(1))
      outList += x
    }
    }
  def unionSet (disjointSet:Array[Int],src:Int,dest:Int):Array[Int]={
    disjointSet(dest) = disjointSet(src)

    for(x <- disjointSet.indices) {
      if (disjointSet(x) == dest)
        {

          disjointSet(x) = disjointSet(src)
        }
    }
    
    println("")
    disjointSet
  }
  println(outList)
  outList
}
}