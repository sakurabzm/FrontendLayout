package de.tkip.sbpm.frontend.LayoutAlgorithm

import de.tkip.sbpm.frontend.LayoutTest

import java.util.concurrent.ConcurrentLinkedQueue
import de.tkip.sbpm.frontend.graph.GraphObject.{Arrow, circle, timeoutSymbol}
import de.tkip.sbpm.frontend.graph.StateGraph

import scala.collection.mutable.{HashMap, HashSet, ListBuffer, Map, Queue, Set, Stack, _}
import scala.math._
import scala.util.control.Breaks._


class RPST(g: RGraph, s2v1: Map[StateGraph, Vertex], v2s1: Map[Vertex, StateGraph], arrowMap1: Map[Int, Arrow]) {

    var initnum = 0
    type SimpleVirtualEdge = (Edge, Component)
    val graph: RGraph = g
    var normalizedGraph: RGraph = new RGraph
    var length = 0
//    lazy val root: Vertex = normalizedGraph.snk
    lazy val bucket = new Array[ListBuffer[Edge]](3 * length + 3)
    lazy val verticesList = new Array[Vertex](length)
    val palmtree = new ListBuffer[Edge]()
    var startPath = new ListBuffer[Edge]()
    var backEdge: Edge = _
    var tStack = Stack[(Int, Vertex, Vertex)]()
    var eStack = Stack[Edge]()
    val eos = (0, new Vertex(""), new Vertex(""))
    val components = new ListBuffer[Component]()
    val virtualEdgeMap: Map[Edge, HashSet[Component]] = Map()
    val componentToTreeNode: Map[Component, TCTreeNode] = Map()
//    val replacedByVirtualEdge: Map[Edge, Edge] = Map()
    var newTreeEdgeList = new ListBuffer[Edge]
    //  var triComponents: Map[EdgeKey, Component] = Map()
    val changedEdgeList = new ListBuffer[Edge] //暂时先试试，万一出了问题再改。
    var triComponents = new ListBuffer[ComponentWithVE]
    var search2num, s = 0
    var veid = 0
    val multiEdges: Map[Edge, ListBuffer[Edge]] = Map()
    val ov2nv: HashMap[Vertex, Vertex] = HashMap.empty[Vertex, Vertex] //origin graph to normalizedGraph
    val ne2oe: Map[Edge, Edge] = Map()
    val extraEdges = new ListBuffer[Edge]
    val visitedTreeArcs = new ListBuffer[Edge]
    val tctree = TCTree()
    var rpstroot: RPSTNode = RPSTNode()
    val rpst = new ListBuffer[(RPSTNode, RPSTNode)]()
    val rpstVertices = new ListBuffer[RPSTNode]()
    var froot = Fragment()
    val vertexnumList = HashSet[Vertex]()
    val v2f: Map[Vertex, Fragment] = Map()
    val s2v: Map[StateGraph, Vertex] = s2v1
    val v2s: Map[Vertex, StateGraph] = v2s1
    val arrowMap: Map[Int, Arrow] = arrowMap1
    var lognum = 0
    //  var triComponents = new ListBuffer[Component]



    def search1(v: Vertex, u: Vertex): Unit = {
        initnum += 1
        v.nd = 1
        v.num = initnum
        verticesList(initnum) = v
        v.lowpt1 = initnum
        v.lowpt2 = initnum

        for (e <- v.adjList) {
            val w = e.getAnotherVertex(v)
            if (w.num == 0) {
                e.isTreeEdge = true
                palmtree += e
                w.father = v.num
                //        if(isStart){
                //          startPath += ((v, w))
                //          println(s"start: ${v.name} -> ${w.name}")
                //          isStart = false
                //        }
                search1(w, v)
                v.nd += w.nd

                if (w.lowpt1 < v.lowpt1) {
                    v.lowpt2 = min(v.lowpt1, w.lowpt2)
                    v.lowpt1 = w.lowpt1
                } else if (v.lowpt1 == w.lowpt1) {
                    v.lowpt2 = min(v.lowpt2, w.lowpt2)
                } else {
                    v.lowpt2 = min(v.lowpt2, w.lowpt1)
                }

            } else if ((v.num > w.num) && (w.num != u.num)) {
                val frond = newEdge(e.to, e.from)
                palmtree += frond
                //        if(isStart){
                //          startPath += ((v, w))
                //          println(s"start: ${v.name} -> ${w.name}")
                //        }else{
                //          isStart = true
                //        }
                if (w.num < v.lowpt1) {
                    v.lowpt2 = v.lowpt1
                    v.lowpt1 = w.num
                } else if (w.num > v.lowpt1) {
                    v.lowpt2 = min(v.lowpt2, w.num)
                }

            }
        }
    }

    def clearNum(): Unit = {
//        for(i <- verticesList.indices){
//            verticesList(i) = null
//        }
//        for(v <- graph.vertices){
//            v.num = -1
//        }
        for (v <- normalizedGraph.vertices) {
            v.num = -1
        }
    }

    def search2(v: Vertex): Unit = {
        v.num = search2num - v.nd + 1
        verticesList(v.num) = v
        v.lowpt1 = v.num
        v.lowpt2 = v.num
        for (e <- v.adjList) {
            if(s == 0){
                s = v.num
                startPath += e
            }
            val w = e.getAnotherVertex(v)
            if (w.num <= 0) {
                search2(w)
                search2num -= 1
                v.addChild(w)

                if (w.lowpt1 < v.lowpt1) {
                    v.lowpt2 = min(v.lowpt1, w.lowpt2)
                    v.lowpt1 = w.lowpt1
                } else if (w.lowpt1 == v.lowpt1) {
                    v.lowpt2 = min(v.lowpt2, w.lowpt2)
                } else {
                    v.lowpt2 = min(v.lowpt2, w.lowpt1)
                }

            } else {
                w.high += v
                if (w.num < v.lowpt1) {
                    v.lowpt2 = v.lowpt1
                    v.lowpt1 = w.num
                } else if (w.num > v.lowpt1) {
                    v.lowpt2 = min(v.lowpt2, w.num)
                }
                s = 0
            }
        }
    }

    def sort(): Unit = {
        for (i <- bucket.indices) {
            bucket(i) = new ListBuffer[Edge]()
        }
//            println("lowpt1: ")
//            normalizedGraph.vertices.foreach({ f =>
//              println(f.name + " -> " + f.lowpt1)
//            })
//            println("******************************************************************")
//            println("lowpt2: ")
//            normalizedGraph.vertices.foreach({ f =>
//              println(f.name + " -> " + f.lowpt2)
//            })

//            println("******************************************************************")
        for (edge <- palmtree) {
            if (edge.isTreeEdge) {
                if (edge.from.num > edge.to.lowpt2) {
                    bucket(3 * edge.to.lowpt1) += edge
                } else {
                    bucket(3 * edge.to.lowpt1 + 2) += edge
                }
            } else {
                bucket((3 * edge.to.num) + 1) += edge
            }
        }

        normalizedGraph.vertices.foreach(_.adjList.clear())

        for (i <- bucket.indices) {
            if (bucket(i).size > 0) {
                for (edge <- bucket(i)) {
                    edge.from.addAdj(edge)
                    edge.from.addAdj1(edge)
                }
            }
        }

//        normalizedGraph.vertices.foreach(v => println(s"adj ${v.num}:${v.adjList}"))
//        normalizedGraph.vertices.foreach(f => f.calFirstChild())
    }


    def pathSearch(v: Vertex): Unit = {

        var h = 0
        var a = new Vertex("")
        var b = new Vertex("")
        for (e <- v.adjList) {
            val w = e.getAnotherVertex(v)
            if (v.num < w.num) {
                visitedTreeArcs += e
                if (startPath.contains(e)) {
                    val deletedFromTstack = ListBuffer[(Int, Vertex, Vertex)]()
                    while (tStack.nonEmpty && tStack.top._2.num > w.lowpt1) {
                        deletedFromTstack += tStack.pop()
                    }
                    if (deletedFromTstack.isEmpty) {
                        tStack.push((w.num + w.nd - 1, verticesList(w.lowpt1), v))
                    } else {
                        val y = deletedFromTstack.maxBy(_._1)._1
                        tStack.push((max(y, w.num + w.nd - 1), verticesList(w.lowpt1), deletedFromTstack.last._3))
                    }
                    tStack.push(eos)
                }
                pathSearch(w)
                val ee = palmtree.find(p => p.from == v && p.to == w).get
                visitedTreeArcs += ee
                eStack.push(ee)
                log(s"estack push1: $ee ---- $palmtree")
                checkType2(v, w)
                checkType1(v, w)
                if(startPath.contains(e)){
                    while(tStack.nonEmpty && !tStack.top.equals(eos)){
                        tStack.pop()
                    }
                    tStack.pop()
                }
                var vhigh = 0
                if(v.high.nonEmpty) {
                    vhigh = v.high.head.num
                }
                while(tStack.nonEmpty && (tStack.top._2.num != v.num) && (tStack.top._3.num != v.num) && (vhigh > tStack.top._1) ){
                    tStack.pop()
                }
            }else{
                if(tStack.nonEmpty){
                    h = tStack.top._1
                    a = tStack.top._2
                    b = tStack.top._3
                }
                if(startPath.contains(e)){
                    val deletedFromTstack = ListBuffer[(Int, Vertex, Vertex)]()
                    while(tStack.nonEmpty && tStack.top._2.num > w.num){
                        deletedFromTstack += tStack.pop()
                    }
                    if(deletedFromTstack.isEmpty){
                        tStack.push((v.num, w, v))
                    }else{
                        val y = deletedFromTstack.maxBy(_._1)._1
                        tStack.push((y, w, deletedFromTstack.last._3))
                    }
                }
                if (w.num == v.father) {
                    val c = new Component
                    addEdgeToComponent(e, c)
                    addEdgeToComponent(w.adjList1.find(e => e.to.equals(v) && e.isVirtual == false).get, c)
                    val ee = newVirtualEdge(newEdge(w, v, true), c)
                    makeTreeEdge(ee._1)
                    components += c
                    log("frond " + w.name + " -> " + v.name)
                } else {
                    val ee = palmtree.find(p => p.from == v && p.to == w).get
                    eStack.push(ee)
                    log(s"estack push2: $ee")
                }
            }
//                    if (e.isHidden == true) {
//                        var pe = replacedByVirtualEdge(e)
//                        while (pe.isHidden) {
//                            pe = replacedByVirtualEdge(pe)
//                        }
//                        eStack.push(pe)
//                    } else {
//                        eStack.push(e)
//                    }
        }

    }

    def checkType1(v: Vertex, w: Vertex): Unit = {
        if (w.lowpt2 >= v.num && w.lowpt1 < v.num && (v.father != 1 || isAdjacentToANotVisitedTreeArc(v))) {
            val c = new Component
            var x = getEstackx()
            var y = getEstacky()
            while ((x >= w.num && x < w.num + w.nd) || (y >= w.num && y < w.num + w.nd)) {
                log(s"debug type1...2... $x -> $y, $v -> $w, wnd: ${w.nd}")
                addEdgeToComponent(eStack.pop(), c)
                x = getEstackx()
                y = getEstacky()
            }

            val wlowpt1 = verticesList(w.lowpt1)
            var ee: SimpleVirtualEdge = newVirtualEdge(newEdge(v, wlowpt1, true), c)
            components += c
            if (eStack.nonEmpty) {
                var e = eStack.top
                if (isSameEdge(e, v, wlowpt1)) {
                    val c = new Component
                    log("debug type1...3... " + getEstackxn() + " -> " + getEstackyn())
                    e = eStack.pop()
                    addEdgeToComponent(e, c)
                    addEdgeToComponent(ee._1, c)
                    log("debug type1...4... " + ee._1.from.name + " -> " + ee._1.to.name)
                    ee = newVirtualEdge(newEdge(wlowpt1, v, true), c)
                    components += c
                    log("type1.1 " + wlowpt1.name + " -> " + v.name)
                }
            }

            if (w.lowpt1 != v.father) {
                eStack.push(ee._1)
                log(s"estack push3: ${ee._1}")
                makeTreeEdge(newEdge(wlowpt1, v))
            } else {
                val c = new Component
                log("debug type1...5... " + wlowpt1.name + " -> " + v.name)
                addEdgeToComponent(ee._1, c)
                addEdgeToComponent(wlowpt1.adjList1.find(e => e.to.equals(v) && e.isVirtual == false).get, c)
                ee = newVirtualEdge(newEdge(wlowpt1, v, true), c)
                makeTreeEdge(ee._1)
                components += c
                log("type1.2 " + wlowpt1.name + " -> " + v.name)
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
        var firstChild = 0
        if(wcopy.children.nonEmpty){
            firstChild = wcopy.children.head.num
        }
//        if(v.num != 1 && (a.num == v.num)){
//            println(s"vnum = anum, ${v.num} = ${a.num}, bfather = ${b.father}")
//            println(s"wdegree = ${w.degree()}. tstack: $tStack")
//        }
        while (v.num != 1 && ((a.num == v.num) || (wcopy.degree() == 2 && firstChild > wcopy.num))) {
            log(s"debug type2...0....")
            log(s"vnum = anum, ${v.num} = ${a.num}, bfather = ${b.father}")
            log(s"wdegree = $w: ${wcopy.degree()}, firstchild: $firstChild. tstack: $tStack")
            if (a.num == v.num && b.father == a.num) {
                tStack.pop()
            } else {
                var eab: Edge = null
                var ee: SimpleVirtualEdge = null
                if (wcopy.degree() == 2 && wcopy.children.head.num > wcopy.num) {
                    log("debug type2...1... " + getEstackxn() + " -> " + getEstackyn())
                    val c = new Component
                    addEdgeToComponent(eStack.pop(), c)
                    val x = eStack.top.to
                    log("debug type2...1... " + getEstackxn() + " -> " + getEstackyn())
                    addEdgeToComponent(eStack.pop(), c)
                    ee = newVirtualEdge(newEdge(v, x, true), c)
                    components += c
                    log(s"type 2.1 ${v.name} -> ${x.name}")
                    if (eStack.nonEmpty) {
                        val e = eStack.top
                        if (isSameEdge(e, x, v)) {
                            eab = eStack.pop()
                        }
                    }
                } else {
                    log(s"found type-2 separation pair: ${v.name}, ${b.name}")
                    h = tStack.top._1
                    a = tStack.top._2
                    b = tStack.top._3
                    tStack.pop()
                    val c = new Component
                    var x = getEstackx()
                    var y = getEstacky()
                    while ((x <= h && x >= a.num) && (y <= h && y >= a.num)) {
                        if (x == a.num && y == b.num) {
                            log("debug type2...2... " + getEstackxn() + " -> " + getEstackyn())
                            eab = eStack.pop()
                            x = getEstackx()
                            y = getEstacky()
                        } else {
                            log("debug type2...3... " + getEstackxn() + " -> " + getEstackyn())
                            addEdgeToComponent(eStack.pop(), c)
                            x = getEstackx()
                            y = getEstacky()
                        }
                    }
                    ee = newVirtualEdge(newEdge(a, b, true), c)
                    components += c
                }
                if (eab != null) {
                    val c = new Component
                    addEdgeToComponent(eab, c)

//                    if (b.num == 0 || isSameEdge(eab, v, verticesList(wcopy.children.head.num))) {
//                        b = verticesList(wcopy.children.head.num)
//                    } else {
//                        b = tStack.top._2
//                    }

                    log("debug type2...4... " + eab.from.name + " -> " + eab.to.name + " type: " + eab.isVirtual)
                    addEdgeToComponent((ee._1), c)
                    log("debug type2...4... " + ee._1.from.name + " -> " + ee._1.to.name + " type: " + ee._1.isVirtual)
                    ee = newVirtualEdge(newEdge(v, b, true), c)
                    components += c
                }
                eStack.push(ee._1)
                log(s"estack push4: ${ee._1}")
                makeTreeEdge(ee._1)
                wcopy = b
                wcopy.father = v.num
                log("type2 " + v.name + " -> " + ee._1.getAnotherVertex(v).name)

            }
            if (tStack.nonEmpty) {
                h = tStack.top._1
                a = tStack.top._2
                b = tStack.top._3
            }else{
                h = 0
                a = new Vertex("")
                b = new Vertex("")
            }
        }
    }

    def computeChildrenandHighv(): Unit ={
        for(i <- 1 to length - 1){
            verticesList(i).high.clear()
            verticesList(i).children.clear()
        }
        computeChildrenandHighv(normalizedGraph.src)
    }

    def computeChildrenandHighv(v: Vertex): Unit ={
        for (e <- v.adjList1) {
            val w = e.getAnotherVertex(v)
            if (v.num < w.num) {
                computeChildrenandHighv(w)
                v.addChild(w)
            }else{
                w.high += v
            }
        }
    }

    def removeEdges(edges: ListBuffer[Edge]): Unit = {
        for (e <- edges) {
            e.from.removeAdj(e)
            e.from.children -= e.to
            e.isHidden = true
        }
    }

//    def addToComponent(edges: ListBuffer[Edge]): Unit = {
//
//    }

    def getEstackx(): Int = {
        if (eStack.isEmpty) {
            0
        } else {
            eStack.top.from.num
        }
    }

    def getEstackxn(): String = {
        if (eStack.isEmpty) {
            "empty"
        } else {
            eStack.top.from.name
        }
    }

    def getEstacky(): Int = {
        if (eStack.isEmpty) {
            0
        } else {
            eStack.top.to.num
        }
    }

    def getEstackyn(): String = {
        if (eStack.isEmpty) {
            "empty"
        } else {
            eStack.top.to.name
        }
    }

    def addEdgeToComponent(e: Edge, c: Component): Unit = {

        //    graphc.removeEdge(e)
        //    e.v.changeFirstChild(e.w)
        if (e.isVirtual) {
            c.virtualEdges += e
        }
        palmtree -= e

        if(e.from.num == 4){
            log(s"palmtree: $e ------ $palmtree")
        }

        c.edges += e
        e.from.removeAdj1(e)
//        e.from.removeChild(e.to)
//        e.from.changeHigh(e.to)
        e.from.removeConnects(e)
        e.to.removeConnects(e)
        e.isHidden = true
    }

    def newEdge(v: Vertex, w: Vertex, isVirtual: Boolean = false): Edge = {
        val eid = MetaInfo.getEdgeIdAndAutoIncrement
        new Edge(eid, v, w, isVirtual)
    }

    def newVirtualEdge(edge: Edge, c: Component): SimpleVirtualEdge = {

        val v = edge.from
        val w = edge.to
        veid += 1
        edge.eid = veid
        c.edges += edge
        c.virtualEdges += edge

//        for (e <- c.edges.filter(_.isVirtual==false)) {
//            replacedByVirtualEdge += (e -> edge)
//        }

//        if (checkBond(c.edges)) {
//            c.setComptype(Bond)
//        } else {
//            var isPolygon = true
//            val countNumMap: Map[Int, Int] = Map()
//            for (i <- c.edges) {
//                val vn = i.v.num
//                val wn = i.w.num
//                if (countNumMap.contains(vn)) {
//                    val n = countNumMap(vn)
//                    countNumMap += (vn -> (n + 1))
//                } else {
//                    countNumMap += (vn -> 1)
//                }
//                if (countNumMap.contains(wn)) {
//                    val n = countNumMap(wn)
//                    countNumMap += (wn -> (n + 1))
//                } else {
//                    countNumMap += (wn -> 1)
//                }
//            }
//
//            breakable {
//                for (n <- countNumMap.valuesIterator) {
//                    if (n != 2) {
//                        isPolygon = false
//                        break
//                    }
//                }
//            }
//
//            if (isPolygon) {
//                c.setComptype(Polygon)
//            } else {
//                c.setComptype(Rigid)
//            }
//        }


        v.addConnects(edge)
        w.addConnects(edge)
        v.addAdj1(edge)
        edge.isNotVisited = false
        computeChildrenandHighv()

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

    def makeTreeEdge(e: Edge): Unit = {
        palmtree += e

        if(e.from.num == 4){
            log(s"palmtree make: $e ------$palmtree")
        }
        e.isTreeEdge = true
    }


    def classifyLastComponents(): Unit = {
        val lastComponent = new Component
        if (eStack.nonEmpty) {
            while (eStack.nonEmpty) {
                val e = eStack.pop()
                lastComponent.edges += e
                if (e.isVirtual) {
                    lastComponent.virtualEdges += e
                }
            }
            components += lastComponent
        }

        for(c <- components){
            var cType: NodeType = Rigid
            if (checkBond(c.edges)) {
                cType = Bond
            } else {
                val countNumMap = Map[Int, Int]()
                for (i <- c.edges) {
                    val vn = i.from.num
                    val wn = i.to.num
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
                    cType = Polygon
                }
            }
            c.setComptype(cType)
            println(s"component edges: ${c.edges}, virtual edges: ${c.virtualEdges}, type: ${c.compType}")
        }

            //      for(i <- virtualEdgeMap.indices){
            //        val vi = virtualEdgeMap(i)
            //        updateTricomponents(vi.compType, vi.component)
            //      }
            //
            //      if(lastComponent.edges.nonEmpty){
            //        updateTricomponents(lastType, lastComponent)
            //      }

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
        var toRemove = new ListBuffer[Edge]()

        for(c <- components){
            for(ve <- c.virtualEdges){
                var found = false
                val removed = new ListBuffer[Component]()
                for(c1 <- components - c){
                    if(c1.virtualEdges.contains(ve) && c1.compType.equals(c.compType)){
                        c.edges ++= c1.edges
                        c.virtualEdges ++= c1.virtualEdges
                        c.edges -= ve
                        c.virtualEdges -= ve
                        found = true
                        removed += c1
                    }
                }
                if(found){
                    for(c1 <- components){
                        if(removed.contains(c1.father)){
                            c1.father = c
                        }
                    }
                }
            }
        }
//        for (ve <- virtualEdgeMap.keys) {
//            breakable {
//                val comp1 = virtualEdgeMap(ve).head
//                val comp2 = virtualEdgeMap(ve).last
//                if (comp1.compType.equals(Rigid))
//                    break
//                if (!comp1.compType.equals(comp2.compType))
//                    break
//
//                for (e <- comp2.edges) {
//                    if (e.isVirtual) {
//                        if (e.eid != ve.eid) {
//                            comp1.virtualEdges += e
//                            comp1.edges += e
//                        }
//                    } else {
//                        comp1.edges += e
//                    }
//                }
//
//                while (comp1.edges.contains(ve)) {
//                    comp1.edges -= ve
//                    comp1.virtualEdges -= ve
//                }
//
//                for (c <- virtualEdgeMap) {
//                    if (c._2.contains(comp2)) {
//                        c._2 -= comp2
//                        c._2 += comp1
//                        if (c._2.size == 1) {
//                            toRemove += c._1
//                        }
//                    }
//                }
//                println(s"remove components: $comp2")
//                virtualEdgeMap(ve) -= comp2
//                components -= comp2
//            }
//        }
//
//        for (ve <- toRemove) {
//            virtualEdgeMap.remove(ve)
//        }
//
//        for (c <- components) {
//            val tn = TCTreeNode()
//            tn.virtualEdges = c.virtualEdges.clone()
//            tn.edges = c.edges.clone()
//            tn.ntype = c.compType
//            tctree.vertices += tn
//            componentToTreeNode += (c -> tn)
//        }
//
//        def update(tn1: TCTreeNode, tn2: TCTreeNode): Unit = {
//            if (tctree.adjMap.contains(tn1)) {
//                tctree.adjMap(tn1) += tn2
//            } else {
//                val tl = new HashSet[TCTreeNode]()
//                tl += tn2
//                tctree.adjMap += (tn1 -> tl)
//            }
//            if (tctree.adjMap.contains(tn2)) {
//                tctree.adjMap(tn2) += tn1
//            } else {
//                val set = new HashSet[TCTreeNode]()
//                set += tn1
//                tctree.adjMap += (tn2 -> set)
//            }
//        }
//
//        println(s"UPDATE: $virtualEdgeMap")
//
//        for (k <- virtualEdgeMap.keys) {
//            val comp1 = virtualEdgeMap(k).head
//            val comp2 = virtualEdgeMap(k).last
//            update(componentToTreeNode(comp1), componentToTreeNode(comp2))
//        }
    }

    /*
    *
    */

    def normalizeGraph(g: RGraph): Unit = {
        val sources = new ListBuffer[Vertex]
        val sinks = new ListBuffer[Vertex]
        val mixed = new ListBuffer[Vertex]
        val sameEdges = Map[(Vertex, Vertex), ListBuffer[Edge]]()

        g.vertices.foreach(_.children.clear())

        //Split off multiple edges
        for(e <- g.edges){
            val key = (e.from, e.to)
            val key1 = (e.to, e.from)
            if(!sameEdges.contains(key) && !sameEdges.contains(key1)){
                val el = new ListBuffer[Edge]()
                sameEdges += key -> el
            }
            sameEdges(key) += e
        }
        for(el <- sameEdges.values){
            if(el.length > 1){
                var v1 = el.head.from
                var w1 = el.head.to
                if(v1.num > w1.num){
                    v1 = el.head.to
                    w1 = el.head.from
                }
                veid += 1
                val ve = new Edge(veid, v1, w1, true)
                val edgesList = new ListBuffer[Edge]()
                multiEdges += ve -> edgesList
                for(e <- el){
                    multiEdges(ve) += e
                    e.from.removeAdj(e)
                    e.from.removeConnects(e)
                    e.to.removeConnects(e)
                    g.edges -= e
                }
                v1.addAdj(ve)
                v1.addConnects(ve)
                w1.addConnects(ve)
                g.edges += ve
            }
        }

        //copy vertices
        for (v <- g.vertices) {
            if (g.getIncomingedges(v).isEmpty) {
                sources += v
            }
            if (v.children.isEmpty) {
                sinks += v
            }
            if (g.getIncomingedges(v).size > 1 && v.adjList.size > 1) {
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
            normalizedGraph.addEdge(nsrc, ov2nv(v))
        }

        //introduce single sink
        val nsnk = new Vertex("NSNK")
        normalizedGraph.addVertex(nsnk)
        normalizedGraph.snk = nsnk
        for (v <- sinks) {
            normalizedGraph.addEdge(ov2nv(v), nsnk)
        }

        //split mixed 'gateways', i.e., vertices with multiple inputs and outputs
        for (v <- mixed) {
            val nv = new Vertex(s"*${v.name}*")
            normalizedGraph.addVertex(nv)

            for (ie <- normalizedGraph.getIncomingedges(ov2nv(v))) {
                normalizedGraph.removeEdge(ie)
                val e: Edge = ne2oe.remove(ie).get
                val ne = normalizedGraph.addEdge(ov2nv(e.from), nv)
                ne2oe += ne -> e
            }

            extraEdges += normalizedGraph.addEdge(nv, ov2nv(v))
        }

        backEdge = normalizedGraph.addBackEdge(nsnk, nsrc)
//        extraEdges += backEdge
        //test graph
        normalizedGraph = LayoutTest.testGraph()
        normalizedGraph.src = normalizedGraph.vertices.head
        normalizedGraph.snk = normalizedGraph.vertices.last
        normalizedGraph.vertices.foreach(_.adjList.clear())
        normalizedGraph.vertices.foreach(v => v.adjList ++= v.connects)
        //
        tctree.graph = normalizedGraph
        length = normalizedGraph.vertices.length + 1
    }


    def constructTree(): Unit = {
        val rootc: Component = components.find(_.edges.contains(backEdge)).get

        constructTree(rootc)

    }

    def constructTree(c: Component): Unit ={
        for(e <- c.virtualEdges){
            val edges = virtualEdgeMap(e)
            for(c1 <- edges){
                if(!c1.equals(c) && !c1.equals(c.father)){
                    c.children += c1
                    c1.father = c
                    constructTree(c1)
                }
            }
        }
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
                val l = new ListBuffer[TCTreeEdge]()
                l += e
                tctree.verticesToEdges += (v -> l)
            }
        }

    }

    def removeTreeEdge(n: TCTreeNode, e: Edge): Unit = {
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
        tctree.treeNodes -= n
        tctree.verticesToEdges.remove(n)
        if (!n.ntype.equals(Trivial)) {
            tctree.adjNodes.remove(n)
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
            tctree.treeNodes -= n
            tctree.verticesToEdges.remove(n)
            if (!n.ntype.equals(Trivial)) {
                tctree.adjNodes.remove(n)
                componentToTreeNode.foreach(f => {
                    if (f._2.equals(n)) {
                        componentToTreeNode.remove(f._1)
                    }
                })
            }
        }
    }

    def constructRPST(): Unit = {
//        val toRemove = new HashSet[TCTreeNode]()
//
//        for (n <- tctree.treeNodes) {
//            for (e <- n.edges) {
//                if (extraEdges.contains(e)) {
//                    removeTreeEdge(n, e)
//                    if (n.ntype.equals(Trivial)) {
//                        toRemove += n
//                    }
//                }
//            }
//        }
//        removeTCTreeNodes(toRemove)
//
//        for (n <- tctree.adjNodes.keys) {
//            if (n.children.size == 1) {
//                val child = n.children.head
//                if (n.equals(tctree.root)) {
//                    //reroot暂时用不到，先留着
//                } else {
//                    val parent = n.parent
//                    removeTCTreeNode(n)
//                    addTCTreeEdge(parent, child)
//                }
//            }
//        }
//
//        for (n <- tctree.adjNodes.keys) {
//            if (n.ntype.equals(Polygon) && n.children.isEmpty && n.edges.filter(p => !p.isVirtual).size == 1) {
//                val edge = n.edges.filter(p => !p.isVirtual).head
//                n.parent.edges += edge
//                removeTCTreeNode(n)
//            }
//        }
//
//        val t2r: Map[TCTreeNode, RPSTNode] = Map()
//        if (tctree.edges.isEmpty) {
//            //empty
//        } else {
//            for (e <- tctree.edges) {
//                val source = e.source
//                val target = e.target
//
//                if (target.ntype.equals(Trivial) && target.edges.isEmpty) {
//                    println("EXTRA EDGE HERE!!!!!")
//                }
//                var rsource: RPSTNode = null
//                var rtarget: RPSTNode = null
//                if (t2r.contains(source)) {
//                    rsource = t2r(source)
//                } else {
//                    rsource = RPSTNode()
//                    rsource.tctnode = source
//                    rsource.ntype = source.ntype
//                    t2r += (source -> rsource)
//                    //          if(!rsource.ntype.equals(Trivial)){
//                    rpstVertices += rsource
//                    //          }
//                    constructEdges(rsource, source)
//                    findBoundaryNodes(rsource)
//                }
//
//                if (t2r.contains(target)) {
//                    rtarget = t2r(target)
//                } else {
//                    rtarget = RPSTNode()
//                    rtarget.tctnode = target
//                    rtarget.ntype = target.ntype
//                    t2r += (target -> rtarget)
//                    //          if(!rtarget.ntype.equals(Trivial)){
//                    rpstVertices += rtarget
//                    //          }
//                    constructEdges(rtarget, target)
//                    //          findBoundaryNodes(rtarget)
//                }
//                var check1, check2 = false
//                if (tctree.root.equals(source)) {
//                    check1 = checkRoot(source, rsource, rtarget)
//                }
//                if (tctree.root.equals(target)) {
//                    check2 = checkRoot(source, rtarget, rsource)
//                }
//                if(check1){
//                    rpstVertices -= rsource
//                }else if(check2){
//                    rpstVertices -= rtarget
//                }else{
//                    rsource.children += rtarget
//                    rpst += ((rsource, rtarget))
//                }
//            }
//        }
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

    def constructEdges(rnode: RPSTNode, tnode: TCTreeNode): Unit = {
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

    def bulidAlledges(rnode: RPSTNode): Unit = {
        if(rnode != null){
            if (rnode.children.nonEmpty) {
                rnode.children.foreach(f => bulidAlledges(f))
                rnode.children.foreach(f => {
                    rnode.fragment.edges ++= f.fragment.edges
                })
            }
            findBoundaryNodes(rnode)
        }
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

    def getEdges(v: Vertex): HashSet[Edge] = {
        val result = new HashSet[Edge]()
        for (e <- graph.edges) {
            if (e.from.equals(v) || e.to.equals(v)) {
                result += e
            }
        }
        result
    }

    def getIncomingEdges(v: Vertex): HashSet[Edge] = {
        val result = new HashSet[Edge]()
        for (e <- graph.edges) {
            if (e.to.equals(v)) {
                result += e
            }
        }
        result
    }

    def getOutgoingEdges(v: Vertex): HashSet[Edge] = {
        val result = new HashSet[Edge]()
        for (e <- graph.edges) {
            if (e.from.equals(v)) {
                result += e
            }
        }
        result
    }

    def isSameEdge(e1: Edge, e2: Edge): Boolean = {
        if (e1 == null || e2 == null)
            return false
        (e1.from.num == e2.from.num && e1.to.num == e2.to.num) ||
          (e1.from.num == e2.to.num && e1.to.num == e2.from.num)

    }

    def isSameEdge(e: Edge, v: Vertex, w: Vertex): Boolean = {
        if (e == null || v == null || w == null)
            return false
        (e.from.num == v.num && e.to.num == w.num) ||
          (e.from.num == w.num && e.to.num == v.num)

    }

    def isAdjacentToANotVisitedTreeArc(v: Vertex): Boolean = {
        for(e <- v.adjList){
            if(e.isTreeEdge && !visitedTreeArcs.contains(e)){
                return true
            }
        }
        false
    }

    def checkBond(el: HashSet[Edge]): Boolean = {
        val edge = el.head
        for (e <- el) {
            if (!isSameEdge(e, edge)) {
                return false
            }
        }
        true
    }

    val fragmentsList = new ListBuffer[Fragment]()

    def constructFragments(n: Fragment): Unit = {
        val atomicList = n.content.vertices.clone()
        for(c <- n.content.children){
            if(!c.ntype.equals(Trivial)){
                val f = Fragment()

                f.nodeType = c.ntype
                f.content = c
                n.children += f
                f.father = n
                fragmentsList += f
                atomicList --= c.vertices
                constructFragments(f)
            }
        }
        n.vertices ++= atomicList
        for(a <- atomicList){
            if(vertexnumList.contains(a)){
                for(f <- fragmentsList){
                    if(f.nodeType.equals(Trivial)){
                        if(f.v.equals(a)){
                            n.children += f
                        }
                    }
                }
            }else{
                val f = Fragment()
                f.nodeType = Trivial
                f.v = a
                f.vertices += a
                f.father = n
                f.entryNode = f
                n.children += f
                fragmentsList += f
                v2f += (a -> f)
                vertexnumList += a
            }
        }
        for(f <- fragmentsList){
            if(f.nodeType.equals(Trivial)){
                println(s"entry: ${n.content.entry.num}")
                if(f.v.equals(n.content.entry)){
                    n.entryNode = f
                    f.father = n
                }
                if(f.v.equals(n.content.exit)){
                    n.exitNode = f
                    f.father = n
                }
            }
        }

        if(!n.nodeType.equals(Trivial)){
            println(s"Fragment: ${n.content.entryName}, type: ${n.nodeType}, children: ${n.children}")
        }

    }

    def log(s:String): Unit ={
        lognum += 1
        print(lognum + " ")
        println(s)
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


    def start(): Unit = {
        val superroot = new Vertex("superroot")
        superroot.num = -1
        superroot.vtype = "superroot"
        normalizeGraph(graph)
        search1(normalizedGraph.src, superroot)
        sort()
        search2num = normalizedGraph.vertices.length
        clearNum()
        search2(normalizedGraph.src)
        //  graphc.initGraph(verticeslist, palmtree)
        tStack.push(eos)
        pathSearch(normalizedGraph.src)
        classifyLastComponents()
        buildComponentMap()
        updateTricomponents()
        println("--------components----------")
        components.foreach(f => {
            println(s"v: ${f.virtualEdges.head.from.name} -> w: ${f.virtualEdges.head.to.name}")
            println("components:")
            println(f)
            println("*************************")
        })
        constructTree()
        constructRPST()
        bulidAlledges(rpstroot)


        froot.content = rpstroot
        froot.nodeType = rpstroot.ntype
        fragmentsList += froot
        constructFragments(froot)


        //println("**********************************************")
    }

}
