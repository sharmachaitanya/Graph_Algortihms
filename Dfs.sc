import scala.collection.mutable._
type Graph = Map[Int , List[Int]]

val gra:Graph = Map(1 -> List(2,4),2 -> List(3,5,7,8), 3 ->List(4,2,10,9), 4 -> List(1,3) ,
  5 -> List(6,7) ,6 ->List(),7 ->List(5,8), 8 -> List(2,7),9 ->List(3),10 -> List(3))
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
DFS(1,gra)