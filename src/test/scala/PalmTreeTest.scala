import scala.collection.mutable.{HashMap, HashSet, ListBuffer, Map, Queue, Set, Stack}
import scala.math._
import scala.util.control.Breaks._
import TestGraph._
import de.tkip.sbpm.frontend.LayoutAlgorithm.Vertex

import scala.collection.mutable

object PalmTreeTest extends App {

    var initnum = 0
    type SimpleVirtualEdge = (GEdge, Component)
    val graph = TestGraph().getGraph()
    var normalizedGraph: RGraph = new RGraph
    var length = 20
    lazy val root: Vertex = normalizedGraph.snk
    var bucket = new Array[ListBuffer[GEdge]](3 * length + 2)
    var palmtree = new ListBuffer[GEdge]()
    var startPath = new ListBuffer[(Vertex, Vertex)]()
    var backEdge: GEdge = null
    var tStack = Stack[(Int, Vertex, Vertex)]()
    var eStack = Stack[GEdge]()
    val eos = (0, new Vertex(""), new Vertex(""))
    val components = new ListBuffer[Component]()
    val virtualEdgeMap: Map[GEdge, HashSet[Component]] = Map()
    val componentToTreeNode: Map[Component, TCTreeNode] = Map()
    val replacedByVirtualEdge: Map[GEdge, GEdge] = Map()
    var newTreeEdgeList = new ListBuffer[GEdge]
    //  var triComponents: Map[EdgeKey, Component] = Map()
    val changedEdgeList = new ListBuffer[GEdge] //暂时先试试，万一出了问题再改。
    var triComponents = new ListBuffer[ComponentWithVE]
    var search2num = 0
    var veid = 0
    val ov2nv: HashMap[Vertex, Vertex] = HashMap.empty[Vertex, Vertex]
    val ne2oe: Map[GEdge, GEdge] = Map()
    val extraEdges = new ListBuffer[GEdge]
    var notVisitedTreeArcs: Map[Vertex, Int] = Map()
    var isStart = true
    val tctree = TCTree()
    var rpstroot: RPSTNode = null
    val rpst = new ListBuffer[(RPSTNode, RPSTNode)]()
    val rpstVertices = new ListBuffer[RPSTNode]()
    //  var triComponents = new ListBuffer[Component]
    val treeArcMap: Map[Vertex, GEdge] = Map()

    for (i <- bucket.indices) {
        bucket(i) = new ListBuffer[GEdge]()
    }

    def search1(v: Vertex, u: Vertex): Unit = {
        initnum += 1
        v.nd = 0
        v.num = initnum
        v.lowpt1 = v
        v.lowpt2 = v
        for (e <- v.connects) {
            val w = e.getAnotherVertex(v)
            if (w.num == 0) {
                e.isTreeEdge = true
                if (e.changeDirection(v, w)) {
                    changedEdgeList += e
                }
                palmtree += e
                v.nd += 1
                w.father = v.num
                treeArcMap += (w -> e)
                //        if(isStart){
                //          startPath += ((v, w))
                //          println(s"start: ${v.name} -> ${w.name}")
                //          isStart = false
                //        }
                search1(w, v)
                v.nd += w.nd

                if (v.lowpt1.num > w.lowpt1.num) {
                    v.lowpt2 = getMinNumVertex(v.lowpt1, w.lowpt2)
                    v.lowpt1 = w.lowpt1
                } else if (v.lowpt1 == w.lowpt1) {
                    v.lowpt2 = getMinNumVertex(v.lowpt2, w.lowpt2)
                } else {
                    v.lowpt2 = getMinNumVertex(v.lowpt2, w.lowpt1)
                }

            } else if ((v.num > w.num) && (w.num != u.num) /*|| (v.flag == false)*/ ) {

                if (e.changeDirection(v, w)) {
                    changedEdgeList += e
                }
                palmtree += e
                //        if(isStart){
                //          startPath += ((v, w))
                //          println(s"start: ${v.name} -> ${w.name}")
                //        }else{
                //          isStart = true
                //        }

                if (w.num < v.lowpt1.num) {
                    v.lowpt2 = v.lowpt1
                    v.lowpt1 = w
                } else if (w.num > v.lowpt1.num) {
                    v.lowpt2 = getMinNumVertex(v.lowpt2, w)
                }

            }
            //      else if(w.num == u.num && v.flag == true){
            //        v.flag = false
            //      }
        }
    }

    def getMinNumVertex(v1: Vertex, v2: Vertex): Vertex = {
        if (v1.num < v2.num) {
            return v1
        } else {
            return v2
        }
    }

    def isSameEdge(e1: GEdge, e2: GEdge): Boolean = {
        if (e1 == null || e2 == null)
            return false
        if ((e1.from.num == e2.from.num && e1.to.num == e2.to.num) ||
                (e1.from.num == e2.to.num && e1.to.num == e2.from.num))
            return true
        else
            return false
    }

    def isSameEdge(e: GEdge, v: Vertex, w: Vertex): Boolean = {
        if (e == null || v == null || w == null)
            return false
        if ((e.from.num == v.num && e.to.num == w.num) ||
                (e.from.num == w.num && e.to.num == v.num))
            return true
        else
            return false
    }

    def clearNum(): Unit = {
        for (v <- normalizedGraph.vertices) {
            v.num = -1
        }
        for (nv <- normalizedGraph.vertices) {
            notVisitedTreeArcs += (nv -> 0)
        }
    }

    def search2(v: Vertex): Unit = {
        v.num = search2num - v.nd
        v.lowpt1 = v
        v.lowpt2 = v
        for (e <- v.adjList) {
            val w = e.getAnotherVertex(v)
            if (w.num <= 0) {
                val n = notVisitedTreeArcs(v) + 1
                notVisitedTreeArcs += (v -> n)
                search2(w)
                search2num -= 1
                if (w.lowpt1.num < v.lowpt1.num) {
                    v.lowpt2 = getMinNumVertex(v.lowpt1, w.lowpt2)
                    v.lowpt1 = w.lowpt1
                } else if (w.lowpt1 == v.lowpt1) {
                    v.lowpt2 = getMinNumVertex(v.lowpt2, w.lowpt2)
                } else {
                    v.lowpt2 = getMinNumVertex(v.lowpt2, w.lowpt1)
                }

            } else {
                w.high += v
                if (w.num < v.lowpt1.num) {
                    v.lowpt2 = v.lowpt1
                    v.lowpt1 = w
                } else if (w.num > v.lowpt1.num) {
                    v.lowpt2 = getMinNumVertex(v.lowpt2, w)
                }
            }
        }
    }

    def sort(): Unit = {
        //    println("lowpt1: ")
        //    normalizedGraph.vertices.foreach({ f =>
        //      println(f.name + " -> " + f.lowpt1)
        //    })
        //    println("******************************************************************")
        //    println("lowpt2: ")
        //    normalizedGraph.vertices.foreach({ f =>
        //      println(f.name + " -> " + f.lowpt2)
        //    })
        //    println("******************************************************************")
        for (edge <- palmtree) {
            if (edge.isTreeEdge) {
                if (edge.v.num > edge.w.lowpt2.num) {
                    bucket(3 * edge.w.lowpt1.num) += edge
                } else {
                    bucket(3 * edge.w.lowpt1.num + 2) += edge
                }
            } else {
                bucket((3 * edge.w.num) + 1) += edge
            }
        }
        for (i <- bucket.indices) {
            if (bucket(i).length > 0) {
                for (edge <- bucket(i)) {
                    edge.v.adjList += edge
                }
            }
        }
        normalizedGraph.vertices.foreach(f => f.calFirstChild())
    }


    def findStartPath(v: Vertex): Unit = {

        for (e <- v.adjList) {
            val w = e.getAnotherVertex(v)
            if (v.num < w.num) {
                if (isStart) {
                    startPath += ((v, w))
                    println(s"start: ${v.name} -> ${w.name}")
                    isStart = false
                }
                findStartPath(w)
            } else {
                if (isStart) {
                    startPath += ((v, w))
                    println(s"start: ${v.name} -> ${w.name}")
                } else {
                    isStart = true
                }
            }
        }
    }

    def pathSearch(v: Vertex): Unit = {
        var h = 0
        var a = new Vertex("")
        var b = new Vertex("")
        for (e <- v.adjList) {
            if (e.isNotVisited) {
                //        println(s"search: $e")
                e.isNotVisited = false
                val w = e.getAnotherVertex(v)
                if (tStack.isEmpty) {
                    h = 0
                    a = new Vertex("")
                    b = new Vertex("")
                } else {
                    h = tStack.top._1
                    a = tStack.top._2
                    b = tStack.top._3
                }

                if (v.num < w.num) {
                    val n = notVisitedTreeArcs(v) - 1
                    notVisitedTreeArcs += (v -> n)
                    if (startPath.contains((v, w))) {
                        var deltri = eos
                        var y = -1
                        while (a.num > w.lowpt1.num) {
                            deltri = tStack.pop()
                            y = max(y, deltri._1)
                            if (tStack.isEmpty) {
                                h = 0
                                a = new Vertex("")
                                b = new Vertex("")
                            } else {
                                h = tStack.top._1
                                a = tStack.top._2
                                b = tStack.top._3
                            }
                        }
                        if (deltri.equals(eos)) {
                            val tstackItem = (w.num + w.nd - 1, w.lowpt1, v)
                            tStack.push(tstackItem)
                        } else {
                            val bb = deltri._3
                            //            deltri.foreach(f => if (y < f._1) y = f._1)
                            val tstackItem = (max(y, w.num + w.nd - 1), w.lowpt1, bb)
                            tStack.push(tstackItem)
                        }
                        tStack.push(eos)
                    }

                    pathSearch(w)

                    if (e.isHidden == true) {
                        var pe = replacedByVirtualEdge(e)
                        while (pe.isHidden) {
                            pe = replacedByVirtualEdge(pe)
                        }
                        eStack.push(pe)
                    } else {
                        eStack.push(e)
                    }

                    checkType2(v, w)
                    checkType1(v, w)

                    if (startPath.contains((v, w)) && tStack.nonEmpty) {
                        while (tStack.top.equals(eos)) {
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
                    var highv = 0
                    if (v.high.nonEmpty)
                        highv = v.high.head.num
                    while ((a.num != v.num) && (b.num != v.num) && (highv > h) && tStack.nonEmpty) {
                        tStack.pop()
                        if (tStack.isEmpty) {
                            h = 0
                            a = new Vertex("")
                            b = new Vertex("")
                        } else {
                            h = tStack.top._1
                            a = tStack.top._2
                            b = tStack.top._3
                        }
                    }
                } else {
                    if (startPath.contains((v, w))) {
                        var deltri = eos
                        var y = -1
                        while (a.num > w.num) {
                            deltri = tStack.pop()
                            y = max(y, deltri._1)
                            h = tStack.top._1
                            a = tStack.top._2
                            b = tStack.top._3
                        }
                        if (deltri.equals(eos)) {
                            tStack.push((v.num, w, v))
                        } else {
                            val bb = deltri._3
                            tStack.push((y, w, bb))
                        }
                    }
                    if (w.num == v.father) {
                        var c = new Component
                        println("debug frond..." + e.v.name + " -> " + e.w.name)
                        newComponent(newEdge(w, v), c)
                        println("debug frond..." + w.name + " -> " + v.name)
                        newComponent(newEdge(w, v), c)
                        val ee = newVirtualEdge(newEdge(w, v, true), c)
                        makeTreeEdge(ee._1)
                        println("frond " + w.name + " -> " + v.name)
                    } else {
                        eStack.push(e)
                    }
                }
            }
        }
    }

    def checkType1(v: Vertex, w: Vertex): Unit = {
        if (w.lowpt2.num >= v.num && w.lowpt1.num < v.num && (v.father != 1 || notVisitedTreeArcs(v) > 0)) {
            var c = new Component
            var x = getEstackx()
            var y = getEstacky()
            while (((x >= w.num && x <= w.num + w.nd) || (y >= w.num && y <= w.num + w.nd)) && eStack.nonEmpty) {
                println("debug type1...1... " + getEstackxn() + " -> " + getEstackyn() + " type: " + eStack.top.isVirtual)
                newComponent(eStack.pop(), c)
                x = getEstackx()
                y = getEstacky()
            }

            // v -> lowpt1 and lowpt1 -> v
            var ee: SimpleVirtualEdge = newVirtualEdge(newEdge(v, w.lowpt1, true), c)
            println("type1.0 " + v.name + " -> " + w.lowpt1.name)
            if (eStack.nonEmpty) {
                var e = eStack.top
                if (isSameEdge(e, v, w.lowpt1)) {
                    var c = new Component
                    println("debug type1...2... " + getEstackxn() + " -> " + getEstackyn())
                    e = eStack.pop()
                    newComponent(e, c)
                    newComponent(ee._1, c)
                    println("debug type1...3... " + ee._1.v.name + " -> " + ee._1.w.name)
                    ee = newVirtualEdge(newEdge(w.lowpt1, v, true), c)
                    println("type1.1 " + w.lowpt1.name + " -> " + v.name)
                }
            }

            if (w.lowpt1.num != v.father) {
                eStack.push(ee._1)
                makeTreeEdge(newEdge(w.lowpt1, v))
            } else {
                val c = new Component
                println("debug type1...4... " + w.lowpt1.name + " -> " + v.name)
                val treearc = treeArcMap(v)
                newComponent(treearc, c)
                ee = newVirtualEdge(newEdge(w.lowpt1, v, true), c)
                makeTreeEdge(newEdge(w.lowpt1, v))
                treeArcMap += (v -> ee._1)
                println("type1.2 " + w.lowpt1.name + " -> " + v.name)
            }
        }

    }

    def checkType2(v: Vertex, w: Vertex): Unit = {
        var h = 0
        var a = new Vertex("")
        var b = new Vertex("")
        if (tStack.nonEmpty) {
            h = tStack.top._1
            a = tStack.top._2
            b = tStack.top._3
        }
        var wcopy = w
        var sonOfWcopy = wcopy.getFirstChild()
        while (v.num != 1 && ((a.num == v.num) || (wcopy.degree() == 2 && sonOfWcopy != null && sonOfWcopy.num > wcopy.num))) {

            if (a.num == v.num && b.father == a.num) {
                tStack.pop()
            } else {
                var eab: GEdge = null
                var ee: SimpleVirtualEdge = null
                if (wcopy.degree() == 2 && sonOfWcopy.num > wcopy.num) {
                    println("debug type2...1... " + getEstackxn() + " -> " + getEstackyn())
                    var c = new Component
                    newComponent(eStack.pop(), c)
                    val x = eStack.top.w
                    println("debug type2...1... " + getEstackxn() + " -> " + getEstackyn())
                    newComponent(eStack.pop(), c)
                    ee = newVirtualEdge(newEdge(v, x, true), c)
                    println(s"type 2.1 ${v.name} -> ${x.name}")
                    if (eStack.nonEmpty) {
                        val e = eStack.top
                        if (isSameEdge(e, v, x) || isSameEdge(e, v, sonOfWcopy)) {
                            eab = eStack.pop()
                        }
                    }
                } else {
                    println(s"found type-2 separation pair: ${v.name}, ${b.name}")
                    h = tStack.top._1
                    a = tStack.top._2
                    b = tStack.top._3
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
                            newComponent(eStack.pop(), c)
                            x = getEstackx()
                            y = getEstacky()
                        }
                    }
                    ee = newVirtualEdge(newEdge(a, b, true), c)

                }
                if (eab != null) {
                    var c = new Component
                    newComponent(eab, c)

                    if (b.num == 0 || isSameEdge(eab, v, sonOfWcopy)) {
                        b = sonOfWcopy
                    } else {
                        b = tStack.top._2
                    }

                    println("debug type2...4... " + eab.v.name + " -> " + eab.w.name + " type: " + eab.isVirtual)
                    newComponent((ee._1), c)
                    println("debug type2...4... " + ee._1.v.name + " -> " + ee._1.w.name + " type: " + ee._1.isVirtual)
                    ee = newVirtualEdge(newEdge(v, b, true), c)
                }
                eStack.push(ee._1)
                makeTreeEdge(ee._1)
                wcopy = ee._1.getAnotherVertex(v)
                wcopy.father = v.num
                sonOfWcopy = wcopy.getFirstChild()
                println("type2 " + v.name + " -> " + ee._1.getAnotherVertex(v).name)

            }
            if (tStack.nonEmpty) {
                h = tStack.top._1
                a = tStack.top._2
                b = tStack.top._3
            }
        }
    }

    def removeEdges(edges: ListBuffer[GEdge]): Unit = {
        for (e <- edges) {
            e.from.adjList -= e
            e.from.transitions -= e.to
            e.isHidden = true
        }
    }

    def addToComponent(edges: ListBuffer[GEdge]): Unit = {

    }

    def getEstackx(): Int = {
        if (eStack.isEmpty) {
            return 0
        } else {
            return eStack.top.v.num
        }
    }

    def getEstackxn(): String = {
        if (eStack.isEmpty) {
            return "empty"
        } else {
            return eStack.top.v.name
        }
    }

    def getEstacky(): Int = {
        if (eStack.isEmpty) {
            return 0
        } else {
            return eStack.top.w.num
        }
    }

    def getEstackyn(): String = {
        if (eStack.isEmpty) {
            return "empty"
        } else {
            return eStack.top.w.name
        }
    }

    def newComponent(e: GEdge, c: Component): Unit = {

        //    graphc.removeEdge(e)
        //    e.v.changeFirstChild(e.w)
        if (e.isVirtual) {
            c.virtualEdges += e
        }
        c.edges += e
        e.v.removeAdjs(e)
        e.v.removeHigh(e.w)
        e.v.removeConnects(e)
        e.w.removeConnects(e)
        e.isHidden = true
    }

    def newEdge(v: Vertex, w: Vertex, isVirtual: Boolean = false): GEdge = {
        val eid = MetaInfo.getEdgeIdAndAutoIncrement
        return GEdge(eid, v, w, isVirtual)
    }

    def newVirtualEdge(edge: GEdge, c: Component): SimpleVirtualEdge = {

        val v = edge.v
        val w = edge.w
        var isBond = true
        veid += 1
        edge.eid = veid
        c.edges += edge
        c.virtualEdges += edge

        for (e <- c.edges) {
            replacedByVirtualEdge += (e -> edge)
        }

        breakable {
            for (e <- c.edges) {
                if (!isSameEdge(e, edge)) {
                    isBond = false
                    break
                }
            }
        }

        if (isBond) {
            c.setComptype(Bond)
        } else {
            var isPolygon = true
            var countNumMap: Map[Int, Int] = Map()
            for (i <- c.edges) {
                val vn = i.v.num
                val wn = i.w.num
                if (countNumMap.contains(vn)) {
                    val n = countNumMap(vn)
                    countNumMap += (vn -> (n + 1))
                } else {
                    countNumMap += (vn -> 1)
                }
                if (countNumMap.contains(wn)) {
                    val n = countNumMap(wn)
                    countNumMap += (wn -> (n + 1))
                } else {
                    countNumMap += (wn -> 1)
                }
            }

            breakable {
                for (n <- countNumMap.valuesIterator) {
                    if (n != 2) {
                        isPolygon = false
                        break
                    }
                }
            }

            if (isPolygon) {
                c.setComptype(Polygon)
            } else {
                c.setComptype(Rigid)
            }
        }

        components += c

        v.addConnects(edge)
        w.addConnects(edge)
        v.adjList += edge
        edge.isNotVisited = false

        //    if(!v.adjList.contains(w)) {
        //      var bn = 0
        //      if (w.num < v.num) {
        //        bn = 3 * w.num + 1
        //        v.high += w
        //      } else if (w.lowpt2 < v.num) {
        //        bn = 3 * w.lowpt1
        //      } else {
        //        bn = 3 * w.lowpt1 + 2
        //      }
        //
        //
        //      val fch = v.getFirstChild()
        //      var fchn = 0
        //      if(fch != null){
        //        if(fch.num < v.num){
        //          fchn = 3 * fch.num + 1
        //        }else if(fch.lowpt2 < v.num){
        //          fchn = 3 * fch.lowpt1
        //        }else{
        //          fchn = 3 * fch.lowpt1 + 2
        //        }
        //        if(bn < fchn){
        //          v.firstchild = w
        //        }
        //      }else{
        //        v.firstchild = w
        //      }
        //    }
        (edge, c)
    }

    def makeTreeEdge(e: GEdge): Unit = {
        palmtree += e
        newTreeEdgeList += e
    }


    def classifyLastComponents(): Unit = {
        var lastComponent = new Component
        var lastType: NodeType = Rigid
        var isBond = true
        if (eStack.nonEmpty) {
            while (eStack.nonEmpty) {
                lastComponent.edges += eStack.pop()
            }
            val edge = lastComponent.edges.head
            breakable {
                for (e <- lastComponent.edges) {
                    if (!isSameEdge(e, edge)) {
                        isBond = false
                        break
                    }
                }
            }
            if (isBond) {
                //        lastComponent.edges += edge
                lastType = Bond
            } else {
                var countNumMap: Map[Int, Int] = Map()
                for (i <- lastComponent.edges) {
                    val vn = i.v.num
                    val wn = i.w.num
                    if (countNumMap.contains(vn)) {
                        val n = countNumMap(vn)
                        countNumMap += (vn -> (n + 1))
                    } else {
                        countNumMap += (vn -> 1)
                    }
                    if (countNumMap.contains(wn)) {
                        val n = countNumMap(wn)
                        countNumMap += (wn -> (n + 1))
                    } else {
                        countNumMap += (wn -> 1)
                    }
                }
                var isPolygon = true
                breakable {
                    for (n <- countNumMap.valuesIterator) {
                        if (n != 2) {
                            isPolygon = false
                            break
                        }
                    }
                }

                if (isPolygon) {
                    lastType = Polygon
                }
            }

            for (e <- lastComponent.edges) {
                if (e.isVirtual) {
                    lastComponent.virtualEdges += e
                }
            }

            lastComponent.setComptype(lastType)
            components += lastComponent

            //      for(i <- virtualEdgeMap.indices){
            //        val vi = virtualEdgeMap(i)
            //        updateTricomponents(vi.compType, vi.component)
            //      }
            //
            //      if(lastComponent.edges.nonEmpty){
            //        updateTricomponents(lastType, lastComponent)
            //      }
        }
    }

    def buildComponentMap(): Unit = {
        for (c <- components) {
            for (e <- c.edges) {
                if (e.isVirtual) {
                    if (virtualEdgeMap.contains(e)) {
                        val lc = virtualEdgeMap(e)
                        lc += c
                        virtualEdgeMap += (e -> lc)
                    } else {
                        val lc = new HashSet[Component]()
                        lc += c
                        virtualEdgeMap += (e -> lc)
                    }
                }
            }
        }
    }

    //merge same virtual edge component
    def updateTricomponents(): Unit = {
        var toRemove = new ListBuffer[GEdge]()

        for (ve <- virtualEdgeMap.keys) {
            breakable {
                val comp1 = virtualEdgeMap(ve).head
                val comp2 = virtualEdgeMap(ve).last
                if (comp1.compType.equals(Rigid))
                    break
                if (!comp1.compType.equals(comp2.compType))
                    break

                for (e <- comp2.edges) {
                    if (e.isVirtual) {
                        if (e.eid != ve.eid) {
                            comp1.virtualEdges += e
                            comp1.edges += e
                        }
                    } else {
                        comp1.edges += e
                    }
                }

                while (comp1.edges.contains(ve)) {
                    comp1.edges -= ve
                    comp1.virtualEdges -= ve
                }

                for (c <- virtualEdgeMap) {
                    if (c._2.contains(comp2)) {
                        c._2 -= comp2
                        c._2 += comp1
                        if (c._2.size == 1) {
                            toRemove += c._1
                        }
                    }
                }
                println(s"remove components: $comp2")
                virtualEdgeMap(ve) -= comp2
                components -= comp2
            }
        }

        for (ve <- toRemove) {
            virtualEdgeMap.remove(ve)
        }

        for (c <- components) {
            val tn = TCTreeNode()
            tn.virtualEdges = c.virtualEdges.clone()
            tn.edges = c.edges.clone()
            tn.ntype = c.compType
            tctree.vertices += tn
            componentToTreeNode += (c -> tn)
        }

        def update(tn1: TCTreeNode, tn2: TCTreeNode): Unit = {
            if (tctree.adjMap.contains(tn1)) {
                tctree.adjMap(tn1) += tn2
            } else {
                val tl = new HashSet[TCTreeNode]()
                tl += tn2
                tctree.adjMap += (tn1 -> tl)
            }
            if (tctree.adjMap.contains(tn2)) {
                tctree.adjMap(tn2) += tn1
            } else {
                val set = new HashSet[TCTreeNode]()
                set += tn1
                tctree.adjMap += (tn2 -> set)
            }
        }

        println(s"UPDATE: $virtualEdgeMap")

        for (k <- virtualEdgeMap.keys) {
            val comp1 = virtualEdgeMap(k).head
            val comp2 = virtualEdgeMap(k).last
            update(componentToTreeNode(comp1), componentToTreeNode(comp2))
        }
    }

    def normalizeGraph(g: RGraph): Unit = {
        val sources = new ListBuffer[Vertex]
        val sinks = new ListBuffer[Vertex]
        val mixed = new ListBuffer[Vertex]

        //copy vertices
        for (v <- g.vertices) {
            if (g.getIncomingedges(v).isEmpty) {
                sources += v
            }
            if (v.transitions.isEmpty) {
                sinks += v
            }
            if (g.getIncomingedges(v).size > 1 && v.transitions.size > 1) {
                mixed += v
            }
            ov2nv += v -> normalizedGraph.addVertex(new Vertex(v.name))
        }

        //copy edges
        for (e <- g.edges) {
            ne2oe += (normalizedGraph.addEdge(ov2nv(e.from), ov2nv(e.to)) -> e)
        }

        //introduce single source
        val nsrc = new Vertex("NSRC")
        normalizedGraph.addVertex(nsrc)
        normalizedGraph.src = nsrc
        for (v <- sources) {
            extraEdges += normalizedGraph.addEdge(nsrc, ov2nv(v))
        }

        //introduce single sink
        val nsnk = new Vertex("NSNK")
        normalizedGraph.addVertex(nsnk)
        normalizedGraph.snk = nsnk
        for (v <- sinks) {
            extraEdges += (normalizedGraph.addEdge(ov2nv(v), nsnk))
        }

        //split mixed 'gateways', i.e., vertices with multiple inputs and outputs
        for (v <- mixed) {
            val nv = new Vertex(s"*${v.name}*")
            normalizedGraph.addVertex(nv)

            for (ie <- normalizedGraph.getIncomingedges(ov2nv(v))) {
                normalizedGraph.removeEdge(ie)
                val e: GEdge = ne2oe.remove(ie).get
                val ne = normalizedGraph.addEdge(ov2nv(e.from), nv)
                ne2oe += ne -> e
            }

            extraEdges += normalizedGraph.addEdge(nv, ov2nv(v))
        }

        backEdge = normalizedGraph.addBackEdge(nsnk, nsrc)
        extraEdges += backEdge

        tctree.graph = normalizedGraph
    }


    def constructTree(): Unit = {
        //find root component
        breakable {
            for (n <- tctree.vertices) {
                if (n.edges.contains(backEdge)) {

                    tctree.root = n
                    break
                }
            }
        }
        println("--------constructTree2----------" + "backedge:" + backEdge)
        val q = new Queue[TCTreeNode]()
        q.enqueue(tctree.root)
        val visited = new HashSet[TCTreeNode]()
        visited += tctree.root

        while (!q.isEmpty && tctree.adjMap.nonEmpty) {
            val n = q.dequeue()
            println("--------constructTree3----------" + tctree.adjMap.keys + "     nnnnnnn:" + n)
            val adjs = tctree.adjMap(n)
            adjs --= visited
            for (w <- adjs) {

                addTCTreeEdge(n, w)
                println("DEBUG START-------------------------------------------")
                tctree.adjMap(w) -= n
                println("DEBUG END---------------------------------------------")
                visited += w
                q.enqueue(w)
            }
        }


        println(s"root: $root")

        //build tree edges
        for (tn <- componentToTreeNode.values) {
            for (e <- tn.edges) {
                if (!e.isVirtual) {
                    if (changedEdgeList.contains(e)) {
                        e.restoreDirection(e.w, e.v)
                    }

                    val ntn = TCTreeNode()
                    ntn.ntype = Trivial
                    ntn.edges += e
                    tctree.vertices += ntn
                    addTCTreeEdge(tn, ntn)
                }
            }
        }
        println("")
    }

    def addTCTreeEdge(v: TCTreeNode, w: TCTreeNode): Unit = {
        val nedge = TCTreeEdge()
        nedge.setEdge(v, w)
        tctree.edges += nedge
        updateVerticesToEdgesMap(v, nedge)
        updateVerticesToEdgesMap(w, nedge)
        v.children += w
        w.parent = v


        def updateVerticesToEdgesMap(v: TCTreeNode, e: TCTreeEdge): Unit = {
            if (tctree.verticesToEdges.contains(v)) {
                tctree.verticesToEdges(v) += e
            } else {
                val l = new ListBuffer[TCTreeEdge]
                l += e
                tctree.verticesToEdges += (v -> l)
            }
        }

    }

    def removeTreeEdge(n: TCTreeNode, e: GEdge): Unit = {
        n.edges -= e
        n.virtualEdges -= e
        tctree.graph.edges -= e
    }

    def removeTCTreeNode(n: TCTreeNode): Unit = {
        val edges = tctree.verticesToEdges(n)
        for (e <- edges) {
            val w = e.getAnotherVertex(n)
            tctree.verticesToEdges(w) -= e
        }
        tctree.edges --= edges
        n.parent.children -= n
        tctree.vertices -= n
        tctree.verticesToEdges.remove(n)
        if (!n.ntype.equals(Trivial)) {
            tctree.adjMap.remove(n)
            componentToTreeNode.foreach(f => {
                if (f._2.equals(n)) {
                    componentToTreeNode.remove(f._1)
                }
            })
        }
    }

    def removeTCTreeNodes(ln: HashSet[TCTreeNode]): Unit = {
        for (n <- ln) {
            val edges = tctree.verticesToEdges(n)
            for (e <- edges) {
                val w = e.getAnotherVertex(n)
                tctree.verticesToEdges(w) -= e
            }
            tctree.edges --= edges
            n.parent.children -= n
            tctree.vertices -= n
            tctree.verticesToEdges.remove(n)
            if (!n.ntype.equals(Trivial)) {
                tctree.adjMap.remove(n)
                componentToTreeNode.foreach(f => {
                    if (f._2.equals(n)) {
                        componentToTreeNode.remove(f._1)
                    }
                })
            }
        }
    }

    def constructRPST(): Unit = {
        val toRemove = new HashSet[TCTreeNode]()

        for (n <- tctree.vertices) {
            for (e <- n.edges) {
                if (extraEdges.contains(e)) {
                    removeTreeEdge(n, e)
                    if (n.ntype.equals(Trivial)) {
                        toRemove += n
                    }
                }
            }
        }
        removeTCTreeNodes(toRemove)

        for (n <- tctree.adjMap.keys) {
            if (n.children.size == 1) {
                val child = n.children.head
                if (n.equals(tctree.root)) {
                    //todo reroot暂时用不到，先留着
                } else {
                    val parent = n.parent
                    removeTCTreeNode(n)
                    addTCTreeEdge(parent, child)
                }
            }
        }

        for (n <- tctree.adjMap.keys) {
            if (n.ntype.equals(Polygon) && n.children.isEmpty && n.edges.filter(p => !p.isVirtual).size == 1) {
                val edge = n.edges.filter(p => !p.isVirtual).head
                n.parent.edges += edge
                removeTCTreeNode(n)
            }
        }

        val t2r: Map[TCTreeNode, RPSTNode] = Map()
        if (tctree.edges.isEmpty) {
            //empty
        } else {
            for (e <- tctree.edges) {
                val source = e.source
                val target = e.target

                if (target.ntype.equals(Trivial) && target.edges.isEmpty) {
                    println("EXTRA EDGE HERE!!!!!")
                }
                var rsource: RPSTNode = null
                var rtarget: RPSTNode = null
                if (t2r.contains(source)) {
                    rsource = t2r(source)
                } else {
                    rsource = RPSTNode()
                    rsource.tctnode = source
                    rsource.ntype = source.ntype
                    t2r += (source -> rsource)
                    //          if(!rsource.ntype.equals(Trivial)){
                    rpstVertices += rsource
                    //          }
                    constructFragment(rsource, source)
                    findBoundaryNodes(rsource)
                }

                if (t2r.contains(target)) {
                    rtarget = t2r(target)
                } else {
                    rtarget = RPSTNode()
                    rtarget.tctnode = target
                    rtarget.ntype = target.ntype
                    t2r += (target -> rtarget)
                    //          if(!rtarget.ntype.equals(Trivial)){
                    rpstVertices += rtarget
                    //          }
                    constructFragment(rtarget, target)
                    //          findBoundaryNodes(rtarget)
                }

                var check1, check2 = false
                if (tctree.root.equals(source)) {
                    check1 = checkRoot(source, rsource, rtarget)
                }
                if (tctree.root.equals(target)) {
                    check2 = checkRoot(source, rtarget, rsource)
                }
//                if(rpstroot != null && rpstroot.equals(rsource)){
//                    rpstroot = checkRoot(rsource)
//                }
                if(check1){
                    rpstVertices -= rsource
                }else if(check2){
                    rpstVertices -= rtarget
                }else{
                    rsource.children += rtarget
                    rpst += ((rsource, rtarget))
                }
            }
        }
    }

    def checkRoot(tnode: TCTreeNode, rnode1: RPSTNode, rnode2: RPSTNode): Boolean = {
        if(tnode.children.size == 1){
            rpstroot = rnode2
            return true
        }else {
            rpstroot = rnode1
            rnode1.children += rnode2
            rpst += ((rnode1, rnode2))
            return true
        }
    }

    def checkRoot(node: RPSTNode): RPSTNode = {
        if(node.children.length == 1){
            checkRoot(node.children.head)
        }else{
            node
        }
    }

    def constructFragment(rnode: RPSTNode, tnode: TCTreeNode): Unit = {
        for (e <- tnode.edges) {
            if (ne2oe.contains(e)) {
                val edge = ne2oe(e)
                rnode.fragment.edges += edge
            } else {
                if (!e.isVirtual) {
                    for (k <- ne2oe.keys) {
                        if (isSameEdge(e, k)) {
                            val edge = ne2oe(k)
                            rnode.fragment.edges += edge
                        }
                    }
                }
            }
        }
    }

    def bulidAllFragements(rnode: RPSTNode): Unit = {
        if (rnode.children.nonEmpty) {
            rnode.children.foreach(f => bulidAllFragements(f))
            rnode.children.foreach(f => {
                rnode.fragment.edges ++= f.fragment.edges
            })
        }
        findBoundaryNodes(rnode)
    }

    def findBoundaryNodes(rnode: RPSTNode): Unit = {

        for (e <- rnode.fragment.edges) {
            rnode.vertices += e.from
            rnode.vertices += e.to
        }

        var csrc = 0
        var csnk = 0
        var flag = false
        var fflag = true
        var vv: Vertex = null

        for (v <- rnode.vertices) {
            breakable {
                if (getIncomingEdges(v).isEmpty) {
                    rnode.entry = v
                    csrc += 1
                    fflag = false
                    break
                }
                if (getOutgoingEdges(v).isEmpty) {
                    rnode.exit = v
                    csnk += 1
                    fflag = false
                    break
                }
                if (getEdges(v).subsetOf(rnode.fragment.edges)) {
                    break
                }
                if (flag) {
                    flag = false
                } else if (!flag) {
                    flag = true
                    vv = v
                }
                if (getOutgoingEdges(v).subsetOf(rnode.fragment.edges) ||
                        disJoint(rnode.fragment.edges, getIncomingEdges(v))) {
                    rnode.entry = v
                }
                if (getIncomingEdges(v).subsetOf(rnode.fragment.edges) ||
                        disJoint(rnode.fragment.edges, getOutgoingEdges(v))) {
                    rnode.exit = v
                }
            }
            if (csrc > 1) {
                rnode.entry = null
            }
            if (csnk > 1) {
                rnode.exit = null
            }
            if (flag && fflag) {
                rnode.entry = vv
                rnode.exit = vv
            }
        }
    }

    def disJoint[A](a: HashSet[A], b: HashSet[A]): Boolean = {
        for (e <- b) {
            if (a.contains(e)) {
                return false
            }
        }
        return true
    }

    def getEdges(v: Vertex): HashSet[GEdge] = {
        val result = new HashSet[GEdge]()
        for (e <- graph.edges) {
            if (e.from.equals(v) || e.to.equals(v)) {
                result += e
            }
        }
        result
    }

    def getIncomingEdges(v: Vertex): HashSet[GEdge] = {
        val result = new HashSet[GEdge]()
        for (e <- graph.edges) {
            if (e.to.equals(v)) {
                result += e
            }
        }
        result
    }

    def getOutgoingEdges(v: Vertex): HashSet[GEdge] = {
        val result = new HashSet[GEdge]()
        for (e <- graph.edges) {
            if (e.from.equals(v)) {
                result += e
            }
        }
        result
    }

    val fragmentsList = ListBuffer[Fragment]()
    def constructFragments(n: Fragment): Unit = {
        val atomicList = n.content.vertices.clone()
        for(c <- n.content.children){
            if(!c.ntype.equals(Trivial)){
                val f = Fragment()
                var single = false
                var empty = false
                if(n.nodeType.equals(Polygon) || n.nodeType.equals(Bond)){
                    if(c.ntype.equals(Polygon) || c.ntype.equals(Bond)){
                        if(c.entry != null && c.entry.equals(n.content.entry)){
                            c.removeVertex(c.entry)
                        }
                        if(c.exit != null && c.exit.equals(n.content.exit)){
                            c.removeVertex(c.exit)
                        }
                        if(c.children.size == 1){
                            single = true
                            f.nodeType = c.children.head.ntype
                            f.content = c.children.head
                        }
                        if(c.children.isEmpty){
                            empty = true
                            f.nodeType = Trivial
                            f.v = c.vertices.head
                        }
                    }
                }

                if(!empty && !single){
                    f.nodeType = c.ntype
                    f.content = c
                }
                n.children += f
                fragmentsList += f
                atomicList --= c.vertices
                if(!empty){
                    constructFragments(f)
                }
            }
        }


        for(a <- atomicList){
            val f = Fragment()
            f.nodeType = Trivial
            f.v = a
            n.children += f
            fragmentsList += f
        }

        if(!n.nodeType.equals(Trivial)){
            println(s"Fragment: ${n.content.entryName}, type: ${n.nodeType}, children: ${n.children}")
        }

    }
    //  def findBoundaryNodes(c: ComponentsTreeBranch): Unit ={
    //    val vertexList = mutable.Set[Vertex]()
    //    var in,out = 0
    //    val v1 = c.e.from
    //    val v2 = c.e.to
    //    var entry, exit: Vertex = null
    //    var order = false
    //    for(e <- c.content.edges){
    //      if(e.from.equals(v1)) c.out1 = true
    //      if(e.from.equals(v2)) c.out2 = true
    //      if(e.to.equals(v1)) c.in1 = true
    //      if(e.to.equals(v2)) c.in2 = true
    //    }
    //    if(c.children.nonEmpty) for(child <- c.children){
    //      findBoundaryNodes(child)
    //    }
    //    //compute in and out
    //
    //    for(child <- c.children){
    //      val cv1 = child.e.from
    //      val cv2 = child.e.to
    //      var cin1, cin2, cout1, cout2 = false
    //      for(e <- c.content.edges){
    //        if(!e.equals(child.e)){
    //          if(e.from.equals(cv1)) cin1 = true
    //          if(e.from.equals(cv2)) cin2 = true
    //          if(e.to.equals(cv1)) cout1 = true
    //          if(e.to.equals(cv2)) cout2 = true
    //        }
    //      }
    //      if((child.out1 && !cout1) || !child.in1) child.entry = child.e.from
    //      if((child.in1 && !cin1) || !child.out1) child.exit = child.e.from
    //      if((child.out2 && !cout2) || !child.in2) child.entry = child.e.to
    //      if((child.in2 && !cin2) || !child.out2) child.exit = child.e.to
    //      if(child.entry != null && child.exit != null){
    //        child.isFragment = true
    //        order = true
    //        // Todo If a child component is a fragment, order the child components from entry to exit.
    //        if(child.ctype.equals(Bond)){
    //
    //        }
    //      }
    //    }
    //
    //    if(order){
    //
    //    }
    //  }
    /*
     *Step 3. Restructure the tree of the triconnected components into the tree of the
     *canonical fragments (the RPST).
     */

    var superroot = new Vertex("superroot")

    superroot.num = -1
    normalizeGraph(graph)
    search1(normalizedGraph.snk, superroot)
    sort()
    search2num = normalizedGraph.vertices.size
    clearNum()
    search2(normalizedGraph.snk)
    findStartPath(normalizedGraph.snk)
    //  graphc.initGraph(verticeslist, palmtree)

    normalizedGraph.vertices.foreach(f => println(f.name + " : " + f.num))


    tStack.push(eos)
    pathSearch(normalizedGraph.snk)

    println("--------components----------")
    //    components.foreach(f => {
    //      println(s"v: ${f.virtualEdges.head.v.name} -> w: ${f.virtualEdges.head.w.name}")
    //      println("components:")
    //      println(f)
    //      println("*************************")
    //    })

    classifyLastComponents()
    buildComponentMap()
    updateTricomponents()
    constructTree()
    constructRPST()
    bulidAllFragements(rpstroot)

    var froot = Fragment()
    froot.content = rpstroot
    froot.nodeType = rpstroot.ntype
    fragmentsList += froot
    constructFragments(froot)

     println()

//    println("--------RPST NODES----------")
//    println(s"root: ${rpstroot}")
//    println(s"content: ${rpstroot.vertices}")
//    rpstVertices.foreach(f => {
//        println(s"father: ${f}, son: ${f.children}")
//        println(s"content: ${f.vertices}")
//        println("------------------------------------------------------------------")
//    })
//
//    println("**********************************************")

}
