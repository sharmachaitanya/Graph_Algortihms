import scala.collection.mutable
import scala.collection.mutable.{ListBuffer}

object Kosarju extends App
{
  type Graph = mutable.Map[Int , ListBuffer[Int]]
  val lst = new ListBuffer[Array[Int]]
  val obj = new Kosaraju 
  obj.addEdges(0,2,lst)
  obj.addEdges(0,3,lst)
  obj.addEdges(1,0,lst)
  obj.addEdges(2,1,lst)
  obj.addEdges(3,4,lst)
  obj.addEdges(4,-1,lst)
  val lxst = new ListBuffer[ListBuffer[Int]]
  val graph = obj.graphMaker(lxst,lst)
  val graphT = obj.Transpose(lxst,lst)
  val d = obj.DFS(0,graph)
  for(x <- d )
  {
    print( " strongly connected componets with " + x + " --> ")
    println( obj.DFS(x,graphT) )
  
  }
}

class Kosaraju {
  type Graph = mutable.Map[Int , ListBuffer[Int]]
  
  def addEdges(start:Int,dest:Int,list:ListBuffer[Array[Int]]):ListBuffer[Array[Int]]={
    val temp = new Array[Int](2)
    temp(0) = start
    temp(1) = dest
    list += temp
  }
  def graphMaker(l: ListBuffer[ListBuffer[Int]],lt:ListBuffer[Array[Int]]): Graph = {
    val grap:Graph = mutable.Map()
    for (q <- 0 until 5) {
      val temp2 = new ListBuffer[Int]
      for (y <- lt) {
        if (y(0) == q && y(1)!= -1)
          temp2 += y(1)
      }
      l += temp2
      grap put(q,temp2)
    }
    grap
  }
  def Transpose(l: ListBuffer[ListBuffer[Int]],lt:ListBuffer[Array[Int]]): Graph = {
    val grap:Graph = mutable.Map()
  
    for (q <- 0 until 5) {
      val temp2 = new ListBuffer[Int]
      for (y <- lt) {
  
        if (y(1) == q && y(1)!= -1)
          temp2 += y(0)
  
      }
      l += temp2
      grap put(q,temp2)
    }
    grap
  }
  def DFS(start: Int, g: Graph): ListBuffer[Int]={
    var visited=List(start)
    var result=List(start)
    var temp = new ListBuffer[Int]
    def DFS0(start: Int): Unit={
      for(n<-g(start); if !visited.contains(n)){
        visited=n :: visited
        result=n :: result
        DFS0(n)
        temp += n
      }}
    DFS0(start)
    //result.reverse
    temp += start
  }
}