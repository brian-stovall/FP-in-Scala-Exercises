import datastructures._

object ChapterThree {
  def main(args: Array[String]): Unit = {
    val testList = MyList(1,2,3,4,5,6,7,8,9,10)
    println(MyList.example)
    println(MyList.tail(MyList.example2))
    println(MyList.drop(testList, 3))
    println(MyList.dropWhile(testList, (x: Int) => {x < 6}))
    println(MyList.setHead(testList, 20))
  }
}
