package de.tkip.sbpm.frontend.LayoutAlgorithm

import de.tkip.sbpm.frontend.graph.GraphObject.Arrow
import de.tkip.sbpm.frontend.graph.StateGraph
import de.tkip.sbpm.frontend.pages.InternalBehaviorPage.State

import scala.collection.mutable
import scala.collection.mutable.{HashSet, ListBuffer, Map, Stack}

object Layout {

    sealed trait EdgeType

    case object TreeEdge extends EdgeType

    case object BackEdge extends EdgeType

    case object ForwardEdge extends EdgeType

    case object CrossEdge extends EdgeType

    //    sealed trait FragmentType
    //    case object Atomic extends FragmentType
    //    case object Sequence extends FragmentType
    //    case object Branch extends FragmentType
    //    case object Loop extends FragmentType
    //    case object Unstructured extends FragmentType


    val SPACE = 50
    val STATELENGTH = 80
    val branchingSplit, branchingJoin = ListBuffer[Fragment]()
    val discovered, finished: Map[Vertex, Int] = Map()
    val edgeType: Map[Edge, EdgeType] = Map()
    val width, length: Map[Vertex, Int] = Map()
    val branchHeight, branchWidth: Map[Fragment, Int] = Map()
    val nodeX, nodeY: Map[Fragment, Int] = Map()
    val verticesX, verticesY: Map[Vertex, Int] = Map()
    val edgeX, edgeY: Map[Edge, Int] = Map()
    var spaceX, spaceY = 0
    var exit: Vertex = null
    var rpst: RPST = null
    val manualMap: Map[StateGraph, (Int, Int)] = Map()
    val visitedManual = HashSet[StateGraph]()
    val visitedEntry, visitedExit, isDrawed = HashSet[Fragment]()

    def init(r: RPST): Unit = {
        spaceX = 0
        spaceY = 0
        branchingSplit.clear()
        branchingJoin.clear()
        discovered.clear()
        finished.clear()
        width.clear()
        length.clear()
        branchHeight.clear()
        branchWidth.clear()
        nodeX.clear()
        nodeY.clear()
        visitedEntry.clear()
        visitedExit.clear()
        manualMap.clear()
        visitedManual.clear()

        rpst = r
        rpst.arrowMap.valuesIterator.foreach(_.rpst = true)

    }

    def start(): Unit = {
        println(s"Layout start......... ")
        for (s <- rpst.s2v.keysIterator) {
            if (s.lock) {
                manualMap += (s -> (s.sx, s.sy))
            }
        }
        LayoutFragment(rpst.froot)
        draw()
        for (s <- manualMap.keysIterator) {
            val offsetx = manualMap(s)._1 - s.sx
            val offsety = manualMap(s)._2 - s.sy
            println(s"offset: $offsetx, $offsety")
            LayoutManualNodes(s, offsetx, offsety)
        }
    }

    def LayoutManualNodes(s: StateGraph, offsetx: Int, offsety: Int): Unit = {
        val v = rpst.s2v(s)
        val f = rpst.v2f(v)
        s.sx += offsetx
        s.sy += offsety
        for (eid <- s.arrowsFromSource) {
            val e = rpst.arrowMap(eid)
            e.changeTargetCoordinate(s.sx + 40, s.sy)
        }
        for (eid <- s.arrowsToTarget) {
            val e = rpst.arrowMap(eid)
            e.changeSourceCoordinate(s.sx + 40, s.sy + 40)
        }
        visitedManual += s
        if ((f.father.nodeType.equals(Polygon) || f.father.nodeType.equals(Loop)) && !f.equals(f.father.exitNode)) {
            if (v.children.nonEmpty) {
                val w = v.children.head
                val ws = rpst.v2s(w)
                val cf = rpst.v2f(w)
                println(s"next: ${ws.data.stateName}")
                if (!cf.equals(f.father.exitNode) && !visitedManual.contains(ws)) {
                    if (manualMap.contains(ws)) {
                        val noffsetx = manualMap(ws)._1 - ws.sx
                        val noffsety = manualMap(ws)._2 - ws.sy
                        LayoutManualNodes(ws, noffsetx, noffsety)
                    } else {
                        LayoutManualNodes(ws, offsetx, offsety)
                    }
                }
            }
        }
    }

    def LayoutFragment(node: Fragment): Unit = {
        for (c <- node.children) {
            //            if(c.children.length == 1){
            //                LayoutFragment(c.children.head)
            //            }else{
            LayoutFragment(c)
            //            }
        }
        if (isAtomic(node)) {
            println("Layout Atomic.........")
            LayoutAtomic(node)
        } else if (isSequence(node)) {
            println(s"Layout Sequence......... $node")
            LayoutSequence(node)
        } else if (isBranching(node) == "Bond") {
            println("Layout Branching.........")
            LayoutBranching(node)
        } else if (isBranching(node) == "Loop") {
            println("Layout Loop.........")
            LayoutLoop(node)
        } else {
            println("Layout Unstructured.........")
            //            LayoutUnstructured(node)
        }
    }

    def isAtomic(node: Fragment): Boolean = {
        if (node.nodeType.equals(Trivial))
            return true
        else
            return false
    }

    def isSequence(node: Fragment): Boolean = {
        if (node.nodeType.equals(Polygon))
            return true
        else
            return false
    }

    def isBranching(node: Fragment): String = {
        val isVisit = new HashSet[Vertex]()
        if (node.nodeType.equals(Bond)) {
            val outE = node.content.exit.outEdges
            for (e <- outE) {
                if (e.to.equals(node.content.entry)) {
                    for (w <- rpst.v2s(e.from).arrowsToTarget) {
                        if (rpst.arrowMap(w).data.target == rpst.v2s(e.to).data.id) {
                            println(s"Arrow Found")
                            rpst.arrowMap(w).isBackEdge = true
                        }
                    }

                    println("Loop: " + node)
                    return "Loop"
                }
            }
        }
        return "Bond"
    }

    def LayoutAtomic(node: Fragment): Unit = {
        branchWidth += (node -> spaceX)
        branchHeight += (node -> spaceY)
    }

    def LayoutSequence(node: Fragment): Unit = {

        //        if (node.ntype.equals(Trivial)) {
        //            if (node.equals(rpst.rpstroot)) {
        //                exit = node.exit
        //                verticesY += (exit -> spaceY)
        //            }
        //        }

        var maxHeight, maxWidth, sumHeight, sumWidth, x, y = 0
        var n = node.entryNode
        var children = node.children

        if (node.father != null && node.entryNode.equals(node.father.entryNode) /*&& node.father.children.length > 1*/ ) {
            //            branchHeight += (n -> spaceY)
            //            branchWidth += (n -> spaceX)
            y -= spaceY
            println(s"entry: $node, father: ${node.father}")
        } else {
            visitedEntry += n
        }


        while (n != null) {
            maxWidth = Math.max(maxWidth, getBranchWidth(n))
            sumHeight = sumHeight + getBranchHeight(n)
            nodeY += (n -> y)
            if(n.nodeType.equals(Trivial) && n.v.name.equals("d")){
                println(s"dddddddd: $node, y: $y")
            }
            y += getBranchHeight(n)

            val outE = outEdges(n, node)


            if (outE.nonEmpty && !n.equals(exit)) {
                println(s"out edge of n: ${node} -> ${n}, $y")
                n = outE.head
            } else {
                n = null
            }
        }

        if (node.father != null && node.exitNode.equals(node.father.exitNode)/*&& node.father.children.length > 1*/ ) {
            nodeY += (node.exitNode -> (nodeY(node.exitNode) - spaceY))
            println(s"exit: ${node} -> ${node.exitNode}")
            sumHeight -= spaceY
        } else {
            visitedExit += node.exitNode
        }

        branchHeight += (node -> (sumHeight - spaceY))
        branchWidth += (node -> maxWidth)
    }

    def LayoutBranching(node: Fragment): Unit = {
        if (node.children.length > 1) {
            var sumHeight, sumWidth, maxHeight, maxWidth, x, y = 0
            val entry = node.entryNode
            val exit = node.exitNode

            for (t <- outEdges(entry, node)) {
                val w = t
                sumHeight += getBranchHeight(w)
                sumWidth += getBranchWidth(w)
                println(s"branchheigt $w: ${getBranchHeight(w)}, branchwidth $w: ${getBranchWidth(w)}")
                maxHeight = Math.max(maxHeight, getBranchHeight(w))
                maxWidth = Math.max(maxWidth, getBranchWidth(w))
            }

            //        x = rpst.v2s(entry.v).sx - sumWidth
            x = 0 - (sumWidth / 2).toInt
            y = spaceY

            for (t <- outEdges(entry, node)) {
                var height, width = 0
                val w = t

                height = Math.ceil(getBranchHeight(w) / 2).toInt
                width = Math.ceil(getBranchWidth(w) / 2).toInt
                //      height = getBranchHeight(w)
                //            width = getBranchWidth(w)
                x += width
                nodeX += (w -> x)
                nodeY += (w -> y)

                x += width
                //todo edges
                //            edgeX += ()
            }
            nodeX += (entry -> 0)
            nodeY += (entry -> 0)
            nodeX += (exit -> 0)
            nodeY += (exit -> (y + maxHeight))
            if (!visitedEntry.contains(exit)) {
                maxHeight += spaceY
            }
            branchHeight += (node -> (maxHeight + 2 * spaceY))
            branchWidth += (node -> sumWidth)
        }
    }

    def LayoutLoop(node: Fragment): Unit = {
        node.nodeType = Loop
        var maxHeight, maxWidth, x, y = 0
        var sumHeight = 0 - spaceY
        var sumWidth = 0 - spaceX

        var n = node.entryNode

//        if (node.entryNode.equals(node.father.entryNode) && node.father.children.length > 1) {
//            //            branchHeight += (n -> spaceY)
//            //            branchWidth += (n -> spaceX)
//            y -= spaceY
//            println(s"entry: $n, father: ${node.father}")
//        } else {
//            visitedEntry += n
//        }

        while (n != null) {
            maxWidth = Math.max(maxWidth, branchWidth(n))
            sumHeight = sumHeight + branchHeight(n)
            nodeY += (n -> y)
            //            updateVerticesPosition(n, rpst.v2s(node.entry).sx, "x")
            //            updateVerticesPosition(n, y, "y")
            y += branchHeight(n)

            val outE = outEdges(n, node)
            if (outE.length == 1) {
                if (n.equals(node.exitNode)) {
                    n = null
                } else {
                    n = outE.head
                }
            } else {
                n = null
            }

            //todo edges
            //            for (e <- n.outEdges) {
            //                if (e.to.equals(node.entry)) {
            //                    edgeX += (e -> spaceX)
            //                }
            //            }
        }

//        if (node.exitNode.equals(node.father.exitNode) && node.father.children.length > 1) {
//            nodeY += (node.exitNode -> (nodeY(node.exitNode) - spaceY))
//            println(s"exit: ${node} -> ${node.exitNode}")
//            sumHeight -= spaceY
//        } else {
//            visitedExit += node.exitNode
//        }


        branchHeight += (node -> sumHeight)
        branchWidth += (node -> (maxWidth /*+ spaceX*/))

        //        if (rpst.graph.getIncomingedges(node.entry).length == 2) {
        //            branchingJoin -= node.entry
        //        }
        //
        //        if (node.exit.outEdges.length == 2) {
        //            branchingSplit -= node.exit
        //        }
    }

    def LayoutUnstructured(node: RPSTNode): Unit = {
        IdentifyEdgeTypes(node.entry, 0)
        val topology = TopologicalSort(node)
        LPSTree(topology)
        val entry = node.entry
        ComputeBranchDimension(entry)
        PreliminaryLayout(entry, 0, 0)
    }

    def IdentifyEdgeTypes(v: Vertex, t: Int): Int = {
        var tt = t + 1
        discovered += (v -> tt)
        for (e <- v.outEdges) {
            val w = e.to
            if (!discovered.contains(w)) {
                edgeType += (e -> TreeEdge)
                tt = IdentifyEdgeTypes(w, tt)
            } else if (!finished.contains(w)) {
                edgeType += (e -> BackEdge)
            } else if (discovered(w) > discovered(v)) {
                edgeType += (e -> ForwardEdge)
            } else {
                edgeType += (e -> CrossEdge)
            }
        }
        finished += (v -> (tt + 1))
        return tt + 1
    }

    def TopologicalSort(node: RPSTNode): Array[Vertex] = {
        val inEdges: Map[Vertex, Int] = Map()
        for (v <- node.vertices) {
            inEdges += (v -> 0)
        }
        for (v <- node.vertices) {
            for (e <- v.outEdges) {
                if (!edgeType(e).equals(BackEdge)) {
                    val n = inEdges(v) + 1
                    inEdges += (v -> n)
                }
            }
        }

        val topology = new Array[Vertex](node.vertices.size + 1)
        var i = 0
        topology(i) = node.entry
        while (i != node.vertices.size) {
            val n = topology(i)
            i += 1
            for (e <- n.outEdges) {
                if (!edgeType(e).equals(BackEdge)) {
                    val w = e.to
                    val t = inEdges(w) - 1
                    if (t == 0) {
                        topology(topology.length) = w
                    }
                }
            }
        }
        return topology
    }

    def LPSTree(topology: Array[Vertex]): Unit = {
        for (v <- topology) {
            val wid = width(v) + SPACE
            for (e <- v.outEdges) {
                val w = e.to
                if (length(w) < length(v) + wid) {
                    length(w) = length(v) + wid
                    w.father = v.num
                }
            }
        }
    }

    def PreProcess(sl: ListBuffer[StateGraph], al: List[Arrow]): Unit = {
        val sources = ListBuffer[StateGraph]()
        val sinks = ListBuffer[StateGraph]()
        for (s <- sl) {
            if (s.data.isStartState) {
                sources += s
            }
            if (s.data.stateType == "End") {
                sinks += s
            }
        }
        CalculateSpace(sl, al)
    }

    def CalculateSpace(sl: ListBuffer[StateGraph], al: List[Arrow]): Unit = {
        for (s <- sl) {
            //      s.data.stateName.length
            val w = Math.max(s.data.stateName.size * 10, STATELENGTH)
            val h = 120
            val v = rpst.s2v(s)
            val n = rpst.v2f(v)

            spaceX = Math.max(w, spaceX)
            spaceY = Math.max(h, spaceY)

            branchWidth += (n -> spaceX)
            branchHeight += (n -> spaceY)


            if (s.arrowsToTarget.length > 1) {
                branchingSplit += n
            }

            if (s.arrowsFromSource.length > 1) {
                branchingJoin += n
            }
        }

        for (e <- al) {
            val w = e.data.label.size * 10
            val h = 120
            spaceX = Math.max(w, spaceX)
            spaceY = Math.max(h, spaceY)
            e.setSpaceY(120)
        }
        spaceX += 100
    }

    def ComputeBranchDimension(v: Vertex): Unit = {
        var h, h1, w, w1 = 0
        for (e <- v.outEdges) {
            if (e.to.father == e.from.num) {
                ComputeBranchDimension(e.to)
                h1 = branchHeight(rpst.v2f(e.to))
                w1 = branchWidth(rpst.v2f(e.to))
            } else {

            }
            h = Math.max(h, h1)
            w = w + w1
        }

        h = spaceY + h
        w = Math.max(spaceX, w)

        branchHeight += (rpst.v2f(v) -> h)
        branchWidth += (rpst.v2f(v) -> w)
    }

    def PreliminaryLayout(v: Vertex, x: Int, y: Int): Unit = {
        var h, w = 0
        verticesX += (v -> (x + (branchWidth(rpst.v2f(v)) - spaceX) / 2))
        verticesY += (v -> y)
        for (e <- v.outEdges) {
            if (e.to.father == e.from.num) {
                h += branchHeight(rpst.v2f(e.to))
                w += branchWidth(rpst.v2f(e.to))
            }
        }
        var x1 = x + Math.max(0, (branchWidth(rpst.v2f(v)) - w) / 2)
        var y1 = y + spaceY
        for (e <- v.outEdges) {
            if (e.to.father == e.from.num) {
                PreliminaryLayout(e.to, x, y)
                x1 += branchWidth(rpst.v2f(e.to))
            } else {
                edgeX += (e -> x)
            }
        }
    }

    def updateVerticesPosition(v: Vertex, p: Int, t: String): Unit = {
        if (t == "x") {
            if (!verticesX.contains(v)) {
                verticesX += (v -> p)
            }
        } else if (t == "y") {
            if (!verticesY.contains(v)) {
                verticesY += (v -> p)
            }
        }
    }

    def draw(): Unit = {
        var x, y = 0
        val root = rpst.froot

        drawFragment(root, 700, 130)
        //        drawNode(root.entryNode, 700, 130)
        //        drawNode(root.exitNode, 700, 130)
        drawEdges()
        drawFreeNodes()
    }

    def drawFragment(node: Fragment, x: Int, y: Int): Unit = {
        var xx = x
        var yy = y
        val entry = node.entryNode
        val exit = node.exitNode

        if (node.equals(rpst.rpstroot)) {
            //        yy -= spaceY
        }

        //        if(!isDrawed.contains(entry)){
        //            nodeX(entry) = 0
        //            nodeY(entry) = 0
        //        }

        if (!node.nodeType.equals(Trivial) && nodeX.contains(node)) {
            print(s"  nodeX: ${nodeX(node)}  ")
            xx += nodeX(node)
        }
        if (!node.nodeType.equals(Trivial) && nodeY.contains(node)) {
            println(s"  nodeY: ${nodeY(node)}  ")
            yy += nodeY(node)
        }


        if (entry != null && entry.nodeType.equals(Trivial) && !entry.isDrawed) {
            print(s"draw entry: $entry, x: $x, y: $y")
            drawNode(entry, xx, yy)
            entry.isDrawed = true
        }

        if (exit != null && exit.nodeType.equals(Trivial) && !exit.isDrawed) {
            print(s"draw exit: $exit")
            drawNode(exit, xx, yy)
            exit.isDrawed = true
        }

        //        if (entry != null && entry.nodeType.equals(Trivial)){
        //            if(isDrawed.contains(entry)){
        //
        //            }else{
        //                println(s"drawNode entry: $node")
        //                drawNode(entry, x, y)
        //                isDrawed += entry
        //            }
        //        }
        //
        //        if (exit != null && exit.nodeType.equals(Trivial)){
        //            if(isDrawed.contains(exit)){
        //
        //            }else{
        //                println(s"drawNode exit: $node")
        //                drawNode(exit, x, y)
        //                isDrawed += exit
        //            }
        //        }

        if (node.children != null && node.children.nonEmpty) {
            for (n <- node.children) {
                print(s"drawfragment: $n")
                drawFragment(n, xx, yy)
            }
        }
    }

    def drawFreeNodes(): Unit = {

    }

    def drawNode(f: Fragment, x: Int, y: Int): Unit = {
        val v = f.v
        val g = rpst.v2s(v)
        var sx = x
        var sy = y
        if (nodeX.contains(f)) {
            print(s"  nodeX: ${nodeX(f)}, ")
            sx += nodeX(f)
        }
        if (nodeY.contains(f)) {
            println(s"  nodeY: ${nodeY(f)}, ")
            sy += nodeY(f)
        }
        g.setCoordinate(sx, sy)
        for (eid <- g.arrowsFromSource) {
            val e = rpst.arrowMap(eid)
            e.changeTargetCoordinate(g.sx + 40, g.sy)
        }
        for (eid <- g.arrowsToTarget) {
            val e = rpst.arrowMap(eid)
            e.changeSourceCoordinate(g.sx + 40, g.sy + 40)
        }
        println(s"v: ${v.name} -----  x: ${g.sx},   y: ${g.sy}")

    }


    def drawEdges(): Unit = {
        //主要是处理交叉边的问题
        //    for(v <- rpst.v2s.keys){
        //      for(e <- v.outEdges){
        ////        if(reserved){
        ////          reserved e
        ////        }
        //
        //      }
        //    }
    }

    def getBranchHeight(f: Fragment): Int = {
        if (branchHeight.contains(f)) {
            return branchHeight(f)
        }
        else
            return spaceY
    }

    def getBranchWidth(f: Fragment): Int = {
        if (branchWidth.contains(f)) {
            return branchWidth(f)
        }
        else
            return spaceX
    }

    def outEdges(f: Fragment, p: Fragment): ListBuffer[Fragment] = {
        val outedges = ListBuffer[Fragment]()
        if (f.nodeType.equals(Trivial)) {
            for (e <- f.v.outEdges) {
                if (p.content.fragment.edges.contains(e)) {
                    val t = e.to
                    for (c <- p.children) {
                        if (c.nodeType.equals(Trivial)) {
                            if (c.v.equals(t)) {
                                outedges += c
                            }
                        } else {
                            if (c.content.vertices.contains(t)) {
                                outedges += c
                            }
                        }
                    }
                }
            }
            return outedges
        }
        else {
            val exit = f.content.exit
            if (exit != null && p != null) {
                if (p.content.exit != null && exit.equals(p.content.exit)) {
                    outedges += p.exitNode
                } else {
                    for (c <- (p.children - f)) {
                        for (t <- exit.children) {
                            if (c.nodeType.equals(Trivial)) {
                                if (c.v.equals(t)) {
                                    outedges += c
                                }
                            } else {
                                if (c.content.entry != null && c.content.entry.equals(t)) {
                                    outedges += c
                                }
                            }
                        }

                    }
                }
            }
            return outedges
        }
    }

}
