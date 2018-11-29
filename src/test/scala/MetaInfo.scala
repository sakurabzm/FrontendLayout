import scala.collection.mutable.{ListBuffer, Stack, Map, Set}
import PalmTreeTest.{Vertex, verticeslist, Graph}

object MetaInfo {
  var edgeId = 0
  val graphEdgeIdMap = Map[Int, Int]().withDefaultValue(0)

  def addNewGraph(gid: Int): Unit = {
    graphEdgeIdMap += (gid -> 0)
  }

  def changeEdgeId(gid: Int, eid: Int): Unit ={
    graphEdgeIdMap += (gid -> eid)
  }

  def getEdgeIdAndAutoIncrement(): Int = {
    edgeId += 1
    return edgeId
  }
}
