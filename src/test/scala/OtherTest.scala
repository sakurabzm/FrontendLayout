import scala.collection.mutable.{ListBuffer, Map, Stack}
import scala.util.control.Breaks._



object OtherTest extends App{
  var tStack = Stack[(Int, String, String)]()
  tStack.push((0,"0","02300"))
  tStack.push((1,"1","010"))
  tStack.push((2,"2","020"))
  tStack.push((13,"13","00130"))
  tStack.push((4,"4","030"))
  tStack.push((11,"11","440"))
  tStack.push((6,"0","02140"))
  tStack.push((8,"0","02310"))
  tStack.push((7,"0","021430"))

  var list = ListBuffer[String]()
  list += "a"
  list += "aa"
  list += "aaa"
  list += "aaaa"
  list += "aaaaa"
  list += "aaaaaa"

  list -= "b"
  for (i <- list.indices){
    if(list(i).equals("aaa")){
      list.insert(i, "bb")
      break
    }
    println(list(i))
  }
  var y = tStack.head._1

  var h, a, b = tStack.top
//  tStack.foreach(f => if(y < f._1) y = f._1)

  println(list)
}
