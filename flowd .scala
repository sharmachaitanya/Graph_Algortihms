object flowd extends App
{
  val w :Array[Array[Int]] = Array(Array(0,3,Int.MaxValue - 1000,7),Array(8,0,2,Int.MaxValue - 1000),Array(5,Int.MaxValue - 1000,0,1),Array(2,Int.MaxValue - 1000,Int.MaxValue - 1000,0))
 val obj = new flowd
 val d = obj.floyd(w)
   for(i <- 0 until 4)
  {
    for(j <-0 until 4)
    {
      println(d(i)(j))
      }
    }
}

class flowd  {
  def floyd(graph:Array[Array[Int]]):Array[Array[Int]] = {
  val dist: Array[Array[Int]] =
    Array.ofDim[Int](4,4)
  for(i <- 0 until 4)for(j <- 0 until 4) dist(i)(j) = graph(i)(j)

  for(k <- 0 until 4)
  {
    for(i <- 0 until 4)
  {
    for(j <-0 until 4)
    {
      var temp1 = dist(i)(k)
      var temp2 = dist(k)(j)
      var temp3 = temp1 + temp2


      if (dist(i)(k) + dist(k)(j) < dist(i)(j))
        dist(i)(j) = dist(i)(k) + dist(k)(j)
    }
  }
  }

  dist

}
}