import com.flowtick.graphs.{Graph, Edge}
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.graphml._
import com.flowtick.graphs.algorithm._

import scala.collection.mutable.ListBuffer

object DijkstraGraph extends App {

    // example taken from https://de.wikipedia.org/wiki/Dijkstra-Algorithmus
    //  val cities: Graph[Int, String, Unit] = Graph.from(Set(
    //    n("Frankfurt") --> (85, n("Mannheim")),
    //    n("Frankfurt") --> (217, n("Wuerzburg")),
    //    n("Frankfurt") --> (173, n("Kassel")),
    //    n("Mannheim") --> (80, n("Karlsruhe")),
    //    n("Wuerzburg") --> (186, n("Erfurt")),
    //    n("Wuerzburg") --> (103, n("Nuernberg")),
    //    n("Stuttgart") --> (183, n("Nuernberg")),
    //    n("Kassel") --> (502, n("Muenchen")),
    //    n("Nuernberg") --> (167, n("Muenchen")),
    //    n("Karlsruhe") --> (250, n("Augsburg")),
    //    n("Augsburg") --> (84, n("Muenchen"))))
    class SimpleGraphExample {
        val graph = Graph.from(Set(
            Edge(555, n("A").value, n("B").value),
            Edge(666, n("A").value, n("C").value),
            Edge(777, n("C").value, n("D").value)
        ))

        //    println(graph.edges)
        // Set(A --> B[()], B --> C[()], D --> A[()])
    }

    //  class A1 extends SimpleGraphExample{}
    var a = new SimpleGraphExample
    //    val graphMLCities: Graph[GraphMLEdge[Int], GraphMLNode[String], GraphMLGraph[Unit]] = DijkstraGraph.cities.toGraphML()
    val graphMLCities = a.graph.toGraphML()

    var graphml = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
    graphml += {
        "<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" xmlns:java=\"http://www.yworks.com/xml/yfiles-common/1.0/java\" xmlns:sys=\"http://www.yworks.com/xml/yfiles-common/markup/primitives/2.0\" xmlns:x=\"http://www.yworks.com/xml/yfiles-common/markup/2.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:y=\"http://www.yworks.com/xml/graphml\" xmlns:yed=\"http://www.yworks.com/xml/yed/3\" xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.1/ygraphml.xsd\">"
        "<y:GroupNode>\n"
        "</y:GroupNode>\n"
    }//    println(fromGraphML[Int, String, Unit](graphMLCities.xml.toString()))
    println(s"<?xml version=1.0 encoding=UTF-8 standalone=no?>>")
    //    println(graphMLCities)
}

//trait GraphMLExample {
//  val graphMLCities: Graph[GraphMLEdge[Int], GraphMLNode[String], GraphMLGraph[Unit]] = DijkstraGraph.cities.toGraphML()
//
//  println(fromGraphML[Int, String, Unit](graphMLCities.xml.toString()))
//  println(graphMLCities)
//  // Graph(GraphMLGraph((),Some(G),List()),Map(GraphMLNode(Erfurt,Erfurt, ...
//}