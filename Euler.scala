import scala.util.control.Breaks._
object eulerpathdemo
{
  def main(args:Array[String]): Unit = {
    type Graph = Map[Int, List[Int]]
    type Vertex = Int
    var odd: List[Int] = Nil
    var visited : List[Int] = Nil
    var result : List[Int] = Nil
    var g: Graph = Map(0 -> List(1, 2), 1 -> List(0, 2), 2 -> List(0, 1, 3), 3 -> List(2))
    var g1: Graph = Map(0 -> List(1, 2, 3, 4), 1 -> List(0, 2), 2 -> List(0, 1), 3 -> List(0, 4), 4 -> List(0, 3))
    var g2: Graph = Map(0 -> List(1, 2, 3), 1 -> List(0, 2, 3), 2 -> List(0, 1), 3 -> List(0, 1, 4), 4 -> List(3))
    var count = 0
    var sum = 0

    def euler(): Unit = {
      breakable {
        for (v <- 0 to 3) {
          var adj = g.get(v).toList.head
          var degree = adj.length
          // println("vertex = " + v)
          // println("degree = " + degree)
          sum = sum + degree
          //println("sum = " + sum)
          //odd degree
          if (degree % 2 != 0) {
            //println("odd degree v = " + v)
            odd = v :: odd
            //println("odd= " + odd)
            count = count + 1
            //println("count = " + count)
          }
        }
        if (sum % 2 == 0 && count == 0) {
          println("circuit")
          traverse(0)
          visited = 0 :: visited
          println("visited circuit = " + visited)
          println("result = " + result)
        }
        if (sum % 2 == 0 && count == 2) {
          println("path")
          //println(odd)
          var head = odd.head
          // println("head= "+head)
          traverse(head)
          println("visited path = " + visited)
          println("result = " + result)
        }
        else {
          println("nothing")
        }
      }
      def traverse(u: Int): Unit = {
        result = u :: result
        visited = u :: visited
        var adj: List[Int] = g(u)
        if (adj.isEmpty) {
          println("res= " + result)
          break()
        }
        println(adj)
        adj.foreach(f = v => {
          var adj1 = adj.filter(_ != v)
          var adj2 = g(v)
          var adj3 = adj2.filter(_ != u)
          g = g + (u -> adj1)
          g = g + (v -> adj3)
          println(g)
          traverse(v)
        })
      }
    }
      euler()
  }
}