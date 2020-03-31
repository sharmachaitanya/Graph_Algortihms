import scala.collection.mutable
import scala.collection.mutable.ListBuffer
object bridge extends App
{
  type Graph = mutable.Map[Int , ListBuffer[Int]]
val lst = new ListBuffer[Array[Int]]
  val obj = new bridge
  obj.addEdges(0,1,lst)
  obj.addEdges(0,2,lst)
  obj.addEdges(0,3,lst)
  obj.addEdges(1,0,lst)
  obj.addEdges(1,2,lst)
  obj.addEdges(2,0,lst)
  obj.addEdges(2,1,lst)
  obj.addEdges(3,0,lst)
  obj.addEdges(3,4,lst)
  obj.addEdges(4,3,lst)
  val lxst = new ListBuffer[ListBuffer[Int]]
  val graph = obj.maker(lxst,lst)
  println(obj.bridg(lst,graph))
}

class bridge {
  type Graph = mutable.Map[Int , ListBuffer[Int]]
  def addEdges(start:Int,dest:Int,list:ListBuffer[Array[Int]]):ListBuffer[Array[Int]]={

  val temp = new Array[Int](2)
  temp(0) = start
  temp(1) = dest
  list += temp
}
  def maker(l: ListBuffer[ListBuffer[Int]],lt:ListBuffer[Array[Int]]): Graph = {
  val grap:Graph = mutable.Map()

  for (q <- 0 until 5) {
    val temp2 = new ListBuffer[Int]
    for (y <- lt) {

      if (y(0) == q )
        temp2 += y(1)

    }
    l += temp2
    grap put(q,temp2)
  }
  grap
}
  
  def bridg(adj:ListBuffer[Array[Int]],gra:Graph) =
{
  var output = new ListBuffer[Array[Int]]
  for ( q <- adj)
    {
    

      val adj = gra.get(q(0)).toList.head.filter(_!=q(1))
       var m1 = gra + (q(0) -> adj)
      val adj1 = gra.get(q(1)).toList.head.filter(_!=q(0))
      m1 =  m1 + (q(1) -> adj1)

     println(m1)
      val begin = m1.head._1
      val res = DFS(begin,m1)
      if(res.length < gra.size -1 )
        output += q

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
    println(result.reverse)
    result.reverse

  }

  output
}
  
}