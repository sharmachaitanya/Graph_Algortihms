import scala.collection.mutable
object primeMst extends App {
  
    val obj: SpannigTree = new SpannigTree()

    type Graph = mutable.Map[Int, List[Int]]
    val gra: Graph = mutable.Map(0 -> List(0, 2, 0, 6, 0), 1 -> List(2, 0, 3, 8, 5), 2 -> List(0, 3, 0, 0, 7), 3 -> List(6, 8, 0, 0, 9), 4 -> List(0, 5, 7, 9, 0))
    val w = Array(
      Array(0, 2, 0, 6, 0),
      Array(2, 0, 3, 8, 5),
      Array(0, 3, 0, 0, 7),
      Array(6, 8, 0, 0, 9),
      Array(0, 5, 7, 9, 0),
    )
    obj.primsMst(w)
  
}



class SpannigTree {

  private val V = 5

  def primsMst(graph:Array[Array[Int]]): Unit = {
    val parent = new Array[Int](V)
    val key = new Array[Int](V)
    val mstSet = new Array[Boolean](V)
    for(i <- 0 until V)
    {
      key(i) = Integer.MAX_VALUE
      mstSet(i) = false
    }
    key(0) = 0
    parent(0) = -1
    for(count <- 0 until V -1){
      val u:Int = minKey(key:Array[Int],mstSet:Array[Boolean])
      mstSet(u) = true
      for(v <- 0 until V)
      {
        if (graph(u)(v)!=0 && !mstSet(v) && graph(u)(v) < key(v)){
                parent(v) = u
                key(v) = graph(u)(v)
              }
      }
    }
    printsol(graph,parent)
  }
  def minKey(key:Array[Int],mstSet:Array[Boolean]):Int = {
    var min = Integer.MAX_VALUE
    var min_Index = -1
    for (v <- 0 until V)
    {
      if(!mstSet(v) && key(v) < min)
      {
        min = key(v)
        min_Index = v
      }
    }
    min_Index
  }
  def printsol(graph:Array[Array[Int]],parent:Array[Int]): Unit = {
    println("Edge -> \tWeight")
    for (i <- 1 until V)
      println(parent(i) + " - " + i + "\t" + graph(i)(parent(i)))
  }

}