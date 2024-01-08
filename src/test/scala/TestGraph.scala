
import scala.collection.mutable.ListBuffer


case class TestGraph() {

    val g = new RGraph

    var s = new Vertex("start")
    s.ntype = "s"
    var v0 = new Vertex("g")
    var v1 = new Vertex("a")
    var v2 = new Vertex("b")
    var v3 = new Vertex("c")
    var v4 = new Vertex("i")
    var v5 = new Vertex("d")
    var v6 = new Vertex("j")
    var v7 = new Vertex("h")
    var e = new Vertex("end")
    e.ntype = "t"

    g.createMateInfo()
    g.addVertex(s)
    g.addVertex(v0)
    g.addVertex(v1)
    g.addVertex(v2)
    g.addVertex(v3)
    g.addVertex(v4)
    g.addVertex(v5)
    g.addVertex(v6)
    g.addVertex(v7)
    g.addVertex(e)

    g.addEdge(s, v0)
    g.addEdge(v0, v1)
    g.addEdge(v0, v4)
    g.addEdge(v1, v2)
    g.addEdge(v2, v3)
    g.addEdge(v3, v7)
    g.addEdge(v4, v5)
    g.addEdge(v5, v6)
    g.addEdge(v6, v7)
    g.addEdge(v6, v4)
    g.addEdge(v7, e)


//    var s = new Vertex("s")
//    s.ntype = "s"
//    var v1 = new Vertex("v1")
//    var v2 = new Vertex("v2")
//    var v3 = new Vertex("v3")
//    var v4 = new Vertex("v4")
//    var v5 = new Vertex("v5")
//    var v6 = new Vertex("v6")
//    var v7 = new Vertex("v7")
//    var t = new Vertex("t")
//    t.ntype = "t"
//
//    g.createMateInfo()
//    g.addVertex(s)
//    g.addVertex(v1)
//    g.addVertex(v2)
//    g.addVertex(v3)
//    g.addVertex(v4)
//    g.addVertex(v5)
//    g.addVertex(v6)
//    g.addVertex(v7)
//    g.addVertex(t)
//
//    g.addEdge(s, v1)
//    g.addEdge(s, v2)
//    g.addEdge(v1, v3)
//    g.addEdge(v1, v5)
//    g.addEdge(v3, v4)
//    g.addEdge(v3, v2)
//    g.addEdge(v4, v1)
//    g.addEdge(v4, v2)
//    g.addEdge(v2, v5)
//    g.addEdge(v5, v6)
//    g.addEdge(v5, v7)
//    g.addEdge(v6, v5)
//    g.addEdge(v6, v7)
//    g.addEdge(v7, v5)
//    g.addEdge(v7, t)


    def getGraph(): RGraph = {
        return g
    }

    //  def createGraph(): Unit ={
    //    var v1 = new Vertex("v1")
    //    var v2 = new Vertex("v2")
    //    var v3 = new Vertex("v3")
    //    var v4 = new Vertex("v4")
    //    var v5 = new Vertex("v5")
    //    var v6 = new Vertex("v6")
    //    var v7 = new Vertex("v7")
    //    var v8 = new Vertex("v8")
    //    var v9 = new Vertex("v9")
    //    var v10 = new Vertex("v10")
    //    var v11 = new Vertex("v11")
    //    var v12 = new Vertex("v12")
    //    var v13 = new Vertex("v13")
    //
    //    verticeslist += v1
    //    verticeslist += v2
    //    verticeslist += v3
    //    verticeslist += v4
    //    verticeslist += v5
    //    verticeslist += v6
    //    verticeslist += v7
    //    verticeslist += v8
    //    verticeslist += v9
    //    verticeslist += v10
    //    verticeslist += v11
    //    verticeslist += v12
    //    verticeslist += v13
    //
    //    v1.connects += v2
    //    v1.connects += v4
    //    v1.connects += v8
    //    v1.connects += v12
    //    v1.connects += v13
    //
    //    v2.connects += v1
    //    v2.connects += v3
    //    v2.connects += v13
    //
    //    v3.connects += v2
    //    v3.connects += v4
    //    v3.connects += v13
    //
    //    v4.connects += v1
    //    v4.connects += v3
    //    v4.connects += v5
    //    v4.connects += v6
    //    v4.connects += v7
    //
    //    v5.connects += v4
    //    v5.connects += v6
    //    v5.connects += v7
    //    v5.connects += v8
    //
    //    v6.connects += v4
    //    v6.connects += v5
    //    v6.connects += v7
    //
    //    v7.connects += v4
    //    v7.connects += v5
    //    v7.connects += v6
    //
    //    v8.connects += v1
    //    v8.connects += v5
    //    v8.connects += v9
    //    v8.connects += v11
    //    v8.connects += v12
    //
    //    v9.connects += v8
    //    v9.connects += v10
    //    v9.connects += v11
    //    v9.connects += v12
    //
    //    v10.connects += v9
    //    v10.connects += v11
    //    v10.connects += v12
    //
    //    v11.connects += v8
    //    v11.connects += v9
    //    v11.connects += v10
    //
    //    v12.connects += v1
    //    v12.connects += v8
    //    v12.connects += v9
    //    v12.connects += v10
    //
    //    v13.connects += v1
    //    v13.connects += v2
    //    v13.connects += v3
    //  }
    //
    //  def createGraph2(): Unit ={
    //
    //    var s = new Vertex("s")
    //    s.ntype = "s"
    //    var v1 = new Vertex("v1")
    //    var v2 = new Vertex("v2")
    //    var v3 = new Vertex("v3")
    //    var v4 = new Vertex("v4")
    //    var v5 = new Vertex("v5")
    //    var v6 = new Vertex("v6")
    //    var v7 = new Vertex("v7")
    //    var t = new Vertex("t")
    //    t.ntype = "t"
    //
    //    verticeslist += s
    //    verticeslist += v1
    //    verticeslist += v2
    //    verticeslist += v3
    //    verticeslist += v4
    //    verticeslist += v5
    //    verticeslist += v6
    //    verticeslist += v7
    //    verticeslist += t
    //
    //
    //    s.connects += v1
    //    s.connects += v2
    //    s.connects += t
    //
    //
    //    v1.connects += s
    //    v1.connects += v3
    //    v1.connects += v4
    //    v1.connects += v5
    //
    //    v2.connects += s
    //    v2.connects += v3
    //    v2.connects += v4
    //    v2.connects += v5
    //
    //    v3.connects += v1
    //    v3.connects += v2
    //    v3.connects += v4
    //
    //    v4.connects += v1
    //    v4.connects += v2
    //    v4.connects += v3
    //
    //    v5.connects += v1
    //    v5.connects += v2
    //    v5.connects += v6
    //    v5.connects += v7
    //
    //    v6.connects += v5
    //    v6.connects += v7
    //
    //    v7.connects += v5
    //    v7.connects += v6
    //    v7.connects += t
    //
    //    t.connects += v7
    //    t.connects += s
    //
    //    s.transitions += v1
    //    s.transitions += v2
    //
    //    v1.transitions += v3
    //    v1.transitions += v5
    //
    //    v2.transitions += v5
    //
    //    v3.transitions += v2
    //    v3.transitions += v4
    //
    //    v4.transitions += v1
    //    v4.transitions += v2
    //
    //    v5.transitions += v6
    //    v5.transitions += v7
    //
    //    v6.transitions += v5
    //    v6.transitions += v7
    //
    //    v7.transitions += v5
    //    v7.transitions += t
    //
    //    //    val c = new Component
    //    //    c.edges += ((v5, v6))
    //    //    c.edges += ((v5, v6))
    //    //    c.edges += ((v5, v6))
    //    //    virtualEdgeList += ((v5, v6, c, Bond))
    //    //
    //    //    val c2 = new Component
    //    //    c2.edges += ((v5, v7))
    //    //    c2.edges += ((v5, v7))
    //    //    c2.edges += ((v5, v7))
    //    //    virtualEdgeList += ((v5, v7, c2, Bond))
    //  }
    //
    //  def createGraph3(): Unit ={
    //    var s = new Vertex("s")
    //    var u = new Vertex("u")
    //    var v = new Vertex("v")
    //    var w = new Vertex("w")
    //    var x = new Vertex("x")
    //    var y = new Vertex("y")
    //    var z = new Vertex("z")
    //    var t = new Vertex("t")
    //
    //    verticeslist += s
    //    verticeslist += u
    //    verticeslist += v
    //    verticeslist += w
    //    verticeslist += x
    //    verticeslist += y
    //    verticeslist += z
    //    verticeslist += t
    //
    //    s.connects += u
    //    s.connects += t
    //    u.connects += v
    //    u.connects += w
    //    v.connects += u
    //    v.connects += w
    //    v.connects += x
    //    w.connects += u
    //    w.connects += v
    //    w.connects += x
    //    x.connects += v
    //    x.connects += w
    //    x.connects += y
    //    y.connects += x
    //    y.connects += z
    //    z.connects += y
    //    z.connects += t
    //    t.connects += z
    //    t.connects += s
    //
    //  }
    //
    //  def createGraph4(): Unit ={
    //    var v1 = new Vertex("1")
    //    var v2 = new Vertex("2")
    //    var v3 = new Vertex("3")
    //    var v4 = new Vertex("4")
    //    var v5 = new Vertex("5")
    //    var v6 = new Vertex("6")
    //    var v7 = new Vertex("7")
    //    var v8 = new Vertex("8")
    //    var v9 = new Vertex("9")
    //    var v10 = new Vertex("10")
    //    var v11 = new Vertex("11")
    //    var v12 = new Vertex("12")
    //
    //    verticeslist += v10
    //    verticeslist += v1
    //    verticeslist += v2
    //    verticeslist += v3
    //    verticeslist += v4
    //    verticeslist += v5
    //    verticeslist += v6
    //    verticeslist += v7
    //    verticeslist += v8
    //    verticeslist += v9
    //    verticeslist += v11
    //    verticeslist += v12
    //
    //    v1.connects += v2
    //    v1.connects += v3
    //    v1.connects += v4
    //    v1.connects += v10
    //
    //    v2.connects += v1
    //    v2.connects += v5
    //
    //    v3.connects += v1
    //    v3.connects += v5
    //
    //    v4.connects += v1
    //    v4.connects += v5
    //
    //    v5.connects += v2
    //    v5.connects += v3
    //    v5.connects += v4
    //    v5.connects += v11
    //
    //    v6.connects += v7
    //    v6.connects += v8
    //    v6.connects += v10
    //
    //    v7.connects += v6
    //    v7.connects += v9
    //    v7.connects += v12
    //
    //    v8.connects += v6
    //    v8.connects += v9
    //    v8.connects += v12
    //
    //    v9.connects += v7
    //    v9.connects += v8
    //    v9.connects += v11
    //
    //    v10.connects += v6
    //    v10.connects += v1
    //
    //    v11.connects += v5
    //    v11.connects += v9
    //
    //    v12.connects += v7
    //    v12.connects += v8
    //  }
}
