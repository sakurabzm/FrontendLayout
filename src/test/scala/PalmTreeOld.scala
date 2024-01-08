import scala.collection.mutable.{ListBuffer, Stack, Map}
import scala.math._

object PalmTreeOld extends App {

    var i = 0

    sealed trait TreeType

    case object Arc extends TreeType

    case object Frond extends TreeType

    sealed trait CompType

    case object Bond extends CompType

    case object Polygon extends CompType

    case object Triconnected extends CompType

    sealed trait Node

    case class QNode() extends Node

    case class SNode() extends Node

    case class PNode() extends Node

    case class RNode() extends Node

    sealed trait Tree[+A]

    case class Branch[A](children: ListBuffer[Tree[A]]) extends Tree[A]

    case class ComponentsTreeArc() {
        var children = new ListBuffer[ComponentsTreeArc]()
        var treeArc = new ListBuffer[Component]()
    }

    //init
    type Edge = (Vertex, Vertex)
    type TreeVirtualEdge = (Vertex, Vertex, Component, CompType)
    type VirtualEdge = (Edge, Component)
    val graphc = new Graph
    var length = 13
    lazy val root: Vertex = verticeslist.head
    var verticeslist = new ListBuffer[Vertex]()
    var bucket = new Array[ListBuffer[Edge]](3 * length + 2)
    var palmtree = new ListBuffer[Edge]()
    var startPath = new ListBuffer[(Int, Int)]()
    var tStack = Stack[(Int, Vertex, Vertex)]()
    var eStack = Stack[Edge]()
    val eos = (0, new Vertex(""), new Vertex(""))
    var virtualEdgeList = new ListBuffer[TreeVirtualEdge]
    var newTreeEdgeList = new ListBuffer[Edge]
    var triComponents = new ListBuffer[Component]


    for (i <- bucket.indices) {
        bucket(i) = new ListBuffer[(Vertex, Vertex)]()
    }

    def createGraph(): Unit = {
        var v1 = new Vertex("v1")
        var v2 = new Vertex("v2")
        var v3 = new Vertex("v3")
        var v4 = new Vertex("v4")
        var v5 = new Vertex("v5")
        var v6 = new Vertex("v6")
        var v7 = new Vertex("v7")
        var v8 = new Vertex("v8")
        var v9 = new Vertex("v9")
        var v10 = new Vertex("v10")
        var v11 = new Vertex("v11")
        var v12 = new Vertex("v12")
        var v13 = new Vertex("v13")

        verticeslist += v1
        verticeslist += v2
        verticeslist += v3
        verticeslist += v4
        verticeslist += v5
        verticeslist += v6
        verticeslist += v7
        verticeslist += v8
        verticeslist += v9
        verticeslist += v10
        verticeslist += v11
        verticeslist += v12
        verticeslist += v13

        v1.connects += v2
        v1.connects += v4
        v1.connects += v8
        v1.connects += v12
        v1.connects += v13

        v2.connects += v1
        v2.connects += v3
        v2.connects += v13

        v3.connects += v2
        v3.connects += v4
        v3.connects += v13

        v4.connects += v1
        v4.connects += v3
        v4.connects += v5
        v4.connects += v6
        v4.connects += v7

        v5.connects += v4
        v5.connects += v6
        v5.connects += v7
        v5.connects += v8

        v6.connects += v4
        v6.connects += v5
        v6.connects += v7

        v7.connects += v4
        v7.connects += v5
        v7.connects += v6

        v8.connects += v1
        v8.connects += v5
        v8.connects += v9
        v8.connects += v11
        v8.connects += v12

        v9.connects += v8
        v9.connects += v10
        v9.connects += v11
        v9.connects += v12

        v10.connects += v9
        v10.connects += v11
        v10.connects += v12

        v11.connects += v8
        v11.connects += v9
        v11.connects += v10

        v12.connects += v1
        v12.connects += v8
        v12.connects += v9
        v12.connects += v10

        v13.connects += v1
        v13.connects += v2
        v13.connects += v3
    }

    def createGraph2(): Unit = {

        var s = new Vertex("s")
        s.ntype = "s"
        var v1 = new Vertex("v1")
        var v2 = new Vertex("v2")
        var v3 = new Vertex("v3")
        var v4 = new Vertex("v4")
        var v5 = new Vertex("v5")
        var v6 = new Vertex("v6")
        var v7 = new Vertex("v7")
        var t = new Vertex("t")
        t.ntype = "t"

        verticeslist += s
        verticeslist += v1
        verticeslist += v2
        verticeslist += v3
        verticeslist += v4
        verticeslist += v5
        verticeslist += v6
        verticeslist += v7
        verticeslist += t


        s.connects += v1
        s.connects += v2
        s.connects += t


        v1.connects += s
        v1.connects += v3
        v1.connects += v4
        v1.connects += v5

        v2.connects += s
        v2.connects += v3
        v2.connects += v4
        v2.connects += v5

        v3.connects += v1
        v3.connects += v2
        v3.connects += v4

        v4.connects += v1
        v4.connects += v2
        v4.connects += v3

        v5.connects += v1
        v5.connects += v2
        v5.connects += v6
        v5.connects += v7

        v6.connects += v5
        v6.connects += v7

        v7.connects += v5
        v7.connects += v6
        v7.connects += t

        t.connects += v7
        t.connects += s

        s.transitions += v1
        s.transitions += v2

        v1.transitions += v3
        v1.transitions += v5

        v2.transitions += v5

        v3.transitions += v2
        v3.transitions += v4

        v4.transitions += v1
        v4.transitions += v2

        v5.transitions += v6
        v5.transitions += v7

        v6.transitions += v5
        v6.transitions += v7

        v7.transitions += v5
        v7.transitions += t

        //    val c = new Component
        //    c.edges += ((v5, v6))
        //    c.edges += ((v5, v6))
        //    c.edges += ((v5, v6))
        //    virtualEdgeList += ((v5, v6, c, Bond))
        //
        //    val c2 = new Component
        //    c2.edges += ((v5, v7))
        //    c2.edges += ((v5, v7))
        //    c2.edges += ((v5, v7))
        //    virtualEdgeList += ((v5, v7, c2, Bond))
    }

    def createGraph3(): Unit = {
        var s = new Vertex("s")
        var u = new Vertex("u")
        var v = new Vertex("v")
        var w = new Vertex("w")
        var x = new Vertex("x")
        var y = new Vertex("y")
        var z = new Vertex("z")
        var t = new Vertex("t")

        verticeslist += s
        verticeslist += u
        verticeslist += v
        verticeslist += w
        verticeslist += x
        verticeslist += y
        verticeslist += z
        verticeslist += t

        s.connects += u
        s.connects += t
        u.connects += v
        u.connects += w
        v.connects += u
        v.connects += w
        v.connects += x
        w.connects += u
        w.connects += v
        w.connects += x
        x.connects += v
        x.connects += w
        x.connects += y
        y.connects += x
        y.connects += z
        z.connects += y
        z.connects += t
        t.connects += z
        t.connects += s

    }

    def createGraph4(): Unit = {
        var v1 = new Vertex("1")
        var v2 = new Vertex("2")
        var v3 = new Vertex("3")
        var v4 = new Vertex("4")
        var v5 = new Vertex("5")
        var v6 = new Vertex("6")
        var v7 = new Vertex("7")
        var v8 = new Vertex("8")
        var v9 = new Vertex("9")
        var v10 = new Vertex("10")
        var v11 = new Vertex("11")
        var v12 = new Vertex("12")

        verticeslist += v10
        verticeslist += v1
        verticeslist += v2
        verticeslist += v3
        verticeslist += v4
        verticeslist += v5
        verticeslist += v6
        verticeslist += v7
        verticeslist += v8
        verticeslist += v9
        verticeslist += v11
        verticeslist += v12

        v1.connects += v2
        v1.connects += v3
        v1.connects += v4
        v1.connects += v10

        v2.connects += v1
        v2.connects += v5

        v3.connects += v1
        v3.connects += v5

        v4.connects += v1
        v4.connects += v5

        v5.connects += v2
        v5.connects += v3
        v5.connects += v4
        v5.connects += v11

        v6.connects += v7
        v6.connects += v8
        v6.connects += v10

        v7.connects += v6
        v7.connects += v9
        v7.connects += v12

        v8.connects += v6
        v8.connects += v9
        v8.connects += v12

        v9.connects += v7
        v9.connects += v8
        v9.connects += v11

        v10.connects += v6
        v10.connects += v1

        v11.connects += v5
        v11.connects += v9

        v12.connects += v7
        v12.connects += v8
    }


    case class Graph() {
        var vertices = new ListBuffer[Vertex]()
        var edges = new ListBuffer[Edge]()

        def initGraph(v: ListBuffer[Vertex], e: ListBuffer[Edge]): Unit = {
            vertices = v.clone()
            edges = e.clone()
        }

        def removeEdge(e: Edge): Unit = {
            edges -= e
        }
    }

    case class BoundaryNode() {
        var num, incoming, outgoing = 0
    }

    case class Component() {
        var edges = new ListBuffer[Edge]()
        var boundary = (BoundaryNode, BoundaryNode)
        var entry = BoundaryNode
        var exit = BoundaryNode
        var ttincoming = 0
        var ttoutgoing = 0
    }

    case class Vertex(n: String) {
        def uuid = java.util.UUID.randomUUID().hashCode()

        val name = n
        var num, lowpt1, lowpt2, nd, father = 0
        var firstchild: Vertex = null
        var flag: Boolean = true
        var visited: Boolean = false
        var ntype: String = ""
        var transitions = new ListBuffer[Vertex]()
        var connects = new ListBuffer[Vertex]()
        var adjList = new ListBuffer[Vertex]()
        var high = new ListBuffer[Int]()
        var deg = 0

        def copy(v: Vertex): Unit = {
            num = v.num
            lowpt1 = v.lowpt1
            lowpt2 = v.lowpt2
            high = v.high
            nd = v.nd
            father = v.father
            flag = v.flag
            visited = v.visited
            connects = v.connects
            adjList = v.adjList
            deg = v.deg
        }

        def degree(): Int = {
            return max(0, connects.length + deg)
        }

        def highv(): Int = {
            if (high.nonEmpty)
                return high.head
            else {
                return 0
            }
        }

        def getFirstChild(): Vertex = {
            return firstchild
        }

        def addConnects(v: Vertex): Unit = {
            //      if(!connects.contains(v))
            connects += v
        }

        def removeConnects(v: Vertex): Unit = {
            connects -= v
        }

        def changeFirstChild(v: Vertex): Unit = {
            if (firstchild.equals(v)) {
                if (v.adjList.contains(v)) {
                    if (!firstchild.equals(v.adjList.last)) {
                        for (i <- v.adjList.indices) {
                            if (firstchild.equals(v.adjList(i))) {
                                firstchild = v.adjList(i + 1)
                                return
                            }
                        }
                    } else {
                        firstchild = null
                    }
                }
            }
            //      adjList -= v
        }

        def removeHigh(vn: Int): Unit = {
            high -= vn
        }

        override def equals(o: Any) = o match {
            case that: Vertex => that.hashCode == this.hashCode
            case _ => false
        }

        override def hashCode = uuid

        override def toString: String = {
            "num = " + num + " father = " + father + " nd = " + nd + " lowpt1 = " + lowpt1 + " lowpt2 = " + lowpt2
        }
    }

    def search1(v: Vertex, u: Vertex): Unit = {
        i += 1
        v.nd = 0
        v.num = i
        v.lowpt1 = i
        v.lowpt2 = i
        for (w <- v.connects) {
            if (w.num == 0) {
                palmtree += ((v, w))
                v.nd += 1
                w.father = v.num
                //        v.d += w
                search1(w, v)
                v.nd += w.nd
                //        v.d ++= w.d

                if (v.lowpt1 > w.lowpt1) {
                    v.lowpt2 = min(v.lowpt1, w.lowpt2)
                    v.lowpt1 = w.lowpt1
                } else if (v.lowpt1 == w.lowpt1) {
                    v.lowpt2 = min(v.lowpt2, w.lowpt2)
                } else {
                    v.lowpt2 = min(v.lowpt2, w.lowpt1)
                }

            } else if ((v.num > w.num) && ((w.num != u.num) || (v.flag == false))) {

                palmtree += ((v, w))
                if (w.num < v.lowpt1) {
                    v.lowpt2 = v.lowpt1
                    v.lowpt1 = w.num
                } else if (w.num > v.lowpt1) {
                    v.lowpt2 = min(v.lowpt2, w.num)
                }

            } else if (w.num == u.num && v.flag == true) {
                //        println("check: father v: " + v.num + " son w: " + w.num)
                v.flag = false
            }
        }
    }

    def sort(): Unit = {
        for ((v, w) <- palmtree) {
            if (v.num > w.num) {
                bucket((3 * w.num) + 1) += ((v, w))
                w.high += v.num
            } else if (v.num > w.lowpt2) {
                bucket(3 * w.lowpt1) += ((v, w))
            } else {
                bucket(3 * w.lowpt1 + 2) += ((v, w))
            }
        }
        for (i <- bucket.indices) {
            if (bucket(i).length > 0) {
                for ((v, w) <- bucket(i)) {
                    v.adjList += w
                }
            }
        }
        for (v <- verticeslist) {
            v.firstchild = v.adjList.head
        }
    }

    var start = true

    def findStartPath(v: Vertex): Unit = {

        for (w <- v.adjList) {
            if (v.num < w.num) {
                if (start) {
                    startPath += ((v.num, w.num))
                    start = false
                }
                findStartPath(w)
            } else {
                if (start) {
                    startPath += ((v.num, w.num))
                } else {
                    start = true
                }
            }
        }
    }

    def pathSearch(v: Vertex): Unit = {
        var h = 0
        var a = new Vertex("a")
        var b = new Vertex("b")
        v.visited = true
        for (w <- v.adjList) {
            h = tStack.top._1
            a = tStack.top._2
            b = tStack.top._3

            if (v.num < w.num) {
                if (startPath.contains((v.num, w.num))) {
                    var triDel = false
                    var deltri = Stack[(Int, Vertex, Vertex)]()
                    while (a.num > w.lowpt1) {
                        deltri.push(tStack.pop())
                        h = tStack.top._1
                        a = tStack.top._2
                        b = tStack.top._3
                        triDel = true
                    }
                    if (!triDel) {
                        tStack.push((w.num + w.nd - 1, verticeslist.find(p => p.num == w.lowpt1).get, v))
                    } else {
                        val bb = deltri.top._3
                        val y = deltri.sortBy(_._1).last._1
                        //            deltri.foreach(f => if (y < f._1) y = f._1)
                        tStack.push((max(y, w.num + w.nd - 1), verticeslist.find(p => p.num == w.lowpt1).get, bb))
                    }
                    tStack.push(eos)
                }

                pathSearch(w)
                eStack.push((v, w))
                /*
                * check type2 pairs
                */
                h = tStack.top._1
                a = tStack.top._2
                b = tStack.top._3
                var wcopy = w
                while (v.num != 1 && ((a.num == v.num) || (wcopy.degree() == 2 && wcopy.getFirstChild().num > wcopy.num))) {

                    if (a.num == v.num && b.father == a.num) {
                        tStack.pop()
                    } else {
                        var eab: Edge = null
                        var ee: VirtualEdge = null
                        if (wcopy.degree() == 2 && wcopy.getFirstChild().num > wcopy.num) {
                            println("debug type2...1... " + getEstackxn() + " -> " + getEstackyn())
                            var c = new Component
                            c.edges += newComponent(eStack.pop())
                            val x = eStack.top._2
                            println("debug type2...1... " + getEstackxn() + " -> " + getEstackyn())
                            c.edges += newComponent(eStack.pop())
                            ee = ((v, x), c)
                            x.father = v.num
                            newVirtualEdge(v, x, c)
                            println(s"type 2.1 ${v.name} -> ${x.name}")
                            if (getEstackx() == v.num && getEstacky() == b.num) {
                                eab = eStack.pop()
                            }
                        } else {
                            tStack.pop()
                            var c = new Component
                            var x = getEstackx()
                            var y = getEstacky()
                            while ((x <= h && x >= a.num) && (y <= h && y >= a.num)) {
                                if (x == a.num && y == b.num) {
                                    println("debug type2...2... " + getEstackxn() + " -> " + getEstackyn())
                                    eab = eStack.pop()
                                    x = getEstackx()
                                    y = getEstacky()
                                } else {
                                    println("debug type2...3... " + getEstackxn() + " -> " + getEstackyn())
                                    c.edges += newComponent(eStack.pop())
                                    x = getEstackx()
                                    y = getEstacky()
                                }
                            }
                            ee = ((a, b), c)
                            b.father = a.num
                            newVirtualEdge(a, b, c)

                        }
                        if (eab != null) {
                            var c = new Component
                            c.edges += newComponent(eab)
                            println("debug type2...4... " + eab._1.name + " -> " + eab._2.name)
                            c.edges += newComponent(((ee._1._1, ee._1._2)))
                            println("debug type2...4... " + ee._1._1.name + " -> " + ee._1._2.name)
                            ee = ((v, b), c)
                            newVirtualEdge(v, b, c)
                        }
                        eStack.push((ee._1._1, ee._1._2))
                        makeTreeEdge((v, b))
                        b.father = v.num
                        println("type2 " + v.name + " -> " + b.name)
                        wcopy = b
                    }
                    h = tStack.top._1
                    a = tStack.top._2
                    b = tStack.top._3
                }
                /*
                * type2 finished
                */

                /*
                * check for a type-1 pair
                */
                var notvisited = false
                v.adjList.foreach(f => if (!f.visited) notvisited = true)
                if (wcopy.lowpt2 >= v.num && wcopy.lowpt1 < v.num && (v.father != 1 || notvisited)) {
                    var c = new Component
                    var x = getEstackx()
                    var y = getEstacky()
                    while ((x >= wcopy.num && x <= wcopy.num + wcopy.nd) || (y >= wcopy.num && y <= wcopy.num + wcopy.nd)) {
                        println("debug type1...1... " + getEstackxn() + " -> " + getEstackyn())
                        c.edges += newComponent(eStack.pop())
                        x = getEstackx()
                        y = getEstacky()
                    }

                    // v -> lowpt1 and lowpt1 -> v
                    var lowpt1w = verticeslist.find(p => p.num == wcopy.lowpt1).get
                    var ee: VirtualEdge = ((v, lowpt1w), c)
                    newVirtualEdge(v, lowpt1w, c)
                    println("type1.0 " + v.name + " -> " + lowpt1w.name)

                    if ((x == lowpt1w.num && y == v.num) || (x == v.num && y == lowpt1w.num)) {
                        var c = new Component
                        println("debug type1...2... " + getEstackxn() + " -> " + getEstackyn())
                        eStack.pop()
                        c.edges += newComponent((lowpt1w, v))
                        println("debug type1...3... " + ee._1._1.name + " -> " + ee._1._2.name)
                        c.edges += newComponent((lowpt1w, v))
                        ee = ((lowpt1w, v), c)
                        newVirtualEdge(lowpt1w, v, c)
                        println("type1.1 " + lowpt1w.name + " -> " + v.name)
                    }

                    if (lowpt1w.num != v.father) {
                        eStack.push((ee._1._1, ee._1._2))
                        makeTreeEdge((lowpt1w, v))
                    } else {
                        val c = new Component
                        println("debug type1...4... " + lowpt1w.name + " -> " + v.name)
                        c.edges += newComponent((lowpt1w, v))
                        c.edges += newComponent((lowpt1w, v))
                        ee = ((lowpt1w, v), c)
                        newVirtualEdge(lowpt1w, v, c)
                        makeTreeEdge((lowpt1w, v))
                        println("type1.2 " + lowpt1w.name + " -> " + v.name)
                    }
                }

                /*
                * type1 finished
                */

                if (startPath.contains((v.num, wcopy.num))) {
                    while (!tStack.top.equals(eos)) {
                        tStack.pop()
                    }
                    tStack.pop()
                }
                if (tStack.isEmpty) {
                    h = 0
                    a = new Vertex("")
                    b = new Vertex("")
                } else {
                    h = tStack.top._1
                    a = tStack.top._2
                    b = tStack.top._3
                }
                while ((a.num != v.num) && (b.num != v.num) && (v.highv() > h) && tStack.nonEmpty) {
                    tStack.pop()
                    h = tStack.top._1
                    a = tStack.top._2
                    b = tStack.top._3
                }
            } else {
                val e = (w, v)
                if (startPath.contains((v.num, w.num))) {
                    var triDel = false
                    var deltri = Stack[(Int, Vertex, Vertex)]()
                    while (a.num > w.num) {
                        deltri.push(tStack.pop())
                        h = tStack.top._1
                        a = tStack.top._2
                        b = tStack.top._3
                        triDel = true
                    }
                    if (triDel == false) {
                        tStack.push((v.num, w, v))
                    } else {
                        val bb = deltri.top._3
                        val y = deltri.maxBy(_._1)._1
                        tStack.push((y, w, bb))
                    }
                }
                if (w.num == v.father) {
                    var c = new Component
                    println("debug frond..." + e._1.name + " -> " + e._2.name)
                    c.edges += newComponent((w, v))
                    println("debug frond..." + w.name + " -> " + v.name)
                    c.edges += newComponent((w, v))
                    val ee: Edge = (w, v)
                    newVirtualEdge(w, v, c)
                    makeTreeEdge(ee)
                    println("frond " + w.name + " -> " + v.name)
                } else {
                    eStack.push(e)
                }
            }
        }
    }

    def getEstackx(): Int = {
        if (eStack.isEmpty) {
            return 0
        } else {
            return eStack.top._1.num
        }
    }

    def getEstackxn(): String = {
        if (eStack.isEmpty) {
            return "empty"
        } else {
            return eStack.top._1.name
        }
    }

    def getEstacky(): Int = {
        if (eStack.isEmpty) {
            return 0
        } else {
            return eStack.top._2.num
        }
    }

    def getEstackyn(): String = {
        if (eStack.isEmpty) {
            return "empty"
        } else {
            return eStack.top._2.name
        }
    }

    def newComponent(e: Edge): Edge = {

        graphc.removeEdge(e)
        e._1.changeFirstChild(e._2)
        e._1.removeConnects(e._2)
        e._1.removeHigh(e._2.num)
        e._2.removeConnects(e._1)

        return e
    }


    def newVirtualEdge(vv: Vertex, vw: Vertex, c: Component): Unit = {

        var v = vv
        var w = vw

        //    if(v.num > w.num){
        //      val u = v
        //      v = w
        //      w = u
        //    }

        //    w.father = v.num

        c.edges += ((v, w))
        var e: TreeVirtualEdge = null
        if (c.edges.length > 3) {
            e = (v, w, c, Triconnected)
        } else {
            var vn = 0
            var v1 = c.edges.head._1.num
            for (i <- c.edges) {
                if (i._1.num == v1 || i._2.num == v1) {
                    vn += 1
                }
            }
            if (vn < 3) {
                e = (v, w, c, Polygon)
            } else {
                e = (v, w, c, Bond)
            }
        }

        virtualEdgeList += e

        //    v.deg += 1
        //    w.deg += 1
        //    if(!v.connects.contains(w)){
        v.addConnects(w)
        //    }
        //    if(!w.connects.contains(v)){
        w.addConnects(v)
        //    }
        if (!v.adjList.contains(w)) {
            var bn = 0
            if (w.num < v.num) {
                bn = 3 * w.num + 1
                v.high += w.num
            } else if (w.lowpt2 < v.num) {
                bn = 3 * w.lowpt1
            } else {
                bn = 3 * w.lowpt1 + 2
            }


            val fch = v.getFirstChild()
            var fchn = 0
            if (fch != null) {
                if (fch.num < v.num) {
                    fchn = 3 * fch.num + 1
                } else if (fch.lowpt2 < v.num) {
                    fchn = 3 * fch.lowpt1
                } else {
                    fchn = 3 * fch.lowpt1 + 2
                }
                if (bn < fchn) {
                    v.firstchild = w
                }
            } else {
                v.firstchild = w
            }
        }
    }

    def makeTreeEdge(e: Edge): Unit = {
        palmtree += e
        newTreeEdgeList += e
    }

    /*
    * TODO. need to change to linear time, need to split the tricomponents from sink node
    */
    def buildTriComp(): Unit = {
        var lastComponent = new Component
        var lastType: CompType = Bond
        while (eStack.nonEmpty) {
            lastComponent.edges += eStack.pop()
        }

        if (lastComponent.edges.length > 3) {
            lastType = Triconnected
        } else {
            var vn = 0
            var v1 = lastComponent.edges.head._1.num
            for (i <- lastComponent.edges) {
                if (i._1.num == v1 || i._2.num == v1) {
                    vn += 1
                }
            }
            if (vn < 3 && lastComponent.edges.length == 3) {
                lastType = Polygon
            }
        }


        for (i <- virtualEdgeList.indices) {
            val vi = virtualEdgeList(i)
            if (vi._3.edges.nonEmpty) {
                if ((vi._4.equals(Bond) || vi._4.equals(Polygon))) {
                    for (e <- vi._3.edges) {
                        var merge = false
                        for (j <- i + 1 to virtualEdgeList.length - 1) {
                            val vj = virtualEdgeList(j)
                            if (vj._3.edges.contains(e) && vi._4.equals(vj._4)) {
                                vi._3.edges ++= vj._3.edges
                                vj._3.edges.clear()
                                merge = true
                            }
                        }
                        if (merge) {
                            while (vi._3.edges.contains(e)) {
                                vi._3.edges -= e
                            }
                        }

                    }
                    triComponents += vi._3
                } else {
                    triComponents += vi._3
                }
            }
        }
        triComponents += lastComponent
    }

    def buildComponetsTree(): Unit = {

    }

    /*
    * after bulid tricomponents
    */
    def findBoundaryNodes(): Unit = {

    }

    var superroot = new Vertex("superroot")
    superroot.num = -1

    createGraph2
    search1(verticeslist.head, superroot)
    sort
    findStartPath(verticeslist.head)
    graphc.initGraph(verticeslist, palmtree)
    tStack.push(eos)
    pathSearch(verticeslist.head)

    println("--------components----------")
    virtualEdgeList.foreach(f => {
        println(s"v: ${f._1.name} -> w: ${f._2.name}")
        println("componets:")
        f._3.edges.foreach(g => {
            println(s"v: ${g._1.name} -> w: ${g._2.name}")
        })
        println("*************************")
    })

    buildTriComp()


    //  println("--------transions----------")
    //  verticeslist.foreach(f => {
    //    println("v: " + f.num)
    //    println(f.transition.foreach(g => {
    //      print(g.num + "   ")
    //    }))
    //    println
    //  })
    //  println("--------------------------------------")

    println("--------Tri Components----------")
    triComponents.foreach(f => {
        println("tricomponets:")
        f.edges.foreach(g => {
            println(s"v: ${g._1.name} -> w: ${g._2.name}")
        })
        println("------------------------------------")
    })
    println("**********************************************")


    //  println(palmtree.foreach(f => println(f._1.num + "  " +f._2.num)))
    //  for(v <- vertexlist){
    //    println(v)
    //    print("adj: " )
    //    v.adjList.foreach(f => print("  " + f.num))
    //    println()
    //    println("-------------------------")
    //  }


    //  for(edge <- palmtree.sortBy(_._1.num).reverse){
    //    println(edge._1.num + " -> " + edge._2.num)
    //  }
    //
    //    val c = new Component()
    //    while (!eStack.isEmpty){
    //      c += eStack.pop()
    //    }
    //    c.foreach(f => {
    //      print(f._1.num + " --> " + f._2.num)
    //      println
    //    })
    //
    //  println("--------------------------------------")
    //
    //  virtualEdgeList.foreach(f => {
    //    print(f._1.num + " -> " +   f._2.num)
    //    println()
    //  })

    //  println("--------------------------------------")
    //
    //
    //  verticeslist.foreach(f => {
    //    println("num: " + f.num + " transition: " + f.transition.length)
    //    f.transition.foreach(g => {
    //      println("tran: " + g.num)
    //    })
    //    println("*************")
    //  })
    //
    //  newTreeEdgeList.foreach(f => {
    //    print(f._1.num + " -> " +   f._2.num)
    //    println()
    //  })


}