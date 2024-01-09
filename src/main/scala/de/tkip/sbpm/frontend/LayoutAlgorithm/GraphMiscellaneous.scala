package de.tkip.sbpm.frontend.LayoutAlgorithm

import scala.collection.mutable
import scala.collection.mutable.{HashSet, ListBuffer, Map}
import scala.math.{exp, max}

//class GraphMiscellaneous {
//
//}
sealed trait TreeType

case object Arc extends TreeType

case object Frond extends TreeType

sealed trait NodeType

case object Trivial extends NodeType
case object Bond extends NodeType
case object Loop extends NodeType
case object Polygon extends NodeType
case object Rigid extends NodeType
case object Undefined extends NodeType

sealed trait Node

case class QNode() extends Node

case class SNode() extends Node

case class PNode() extends Node

case class RNode() extends Node

sealed trait Tree[+A]

case class Branch[A](children: ListBuffer[Tree[A]]) extends Tree[A]

case class RGraph() {
    val id = 0
    var vertices = new ListBuffer[Vertex]()
    var freeVertices = new ListBuffer[Vertex]()
    var edges = new ListBuffer[Edge]()
    var src: Vertex = null
    var snk: Vertex = null

    def createMateInfo(): Unit = {
        MetaInfo.addNewGraph(id)
    }

    def addVertex(v: Vertex): Vertex = {
        vertices += v
        return v
    }

    def addEdge(source: Vertex, sink: Vertex): Edge = {
        val eid = MetaInfo.getEdgeIdAndAutoIncrement
        val e = new Edge(eid, source, sink)
        edges += e
//        source.addChild(sink)
        source.addOutEdges(e)
        source.addAdj(e)
        source.addConnects(e)
        sink.addConnects(e)
        sink.addIncomingEdges(e)
        return e
    }

    def addEdgeToHead(source: Vertex, sink: Vertex): Edge = {
        val eid = MetaInfo.getEdgeIdAndAutoIncrement
        val e = new Edge(eid, source, sink)
        edges += e
        source.connects.+=:(e)
        sink.connects.+=:(e)
        return e
    }

    def addBackEdge(source: Vertex, sink: Vertex): Edge = {
        val eid = MetaInfo.getEdgeIdAndAutoIncrement
        snk = source
        src = sink

        val backEdge = new Edge(eid, snk, src)
        edges += backEdge
        source.addAdj(backEdge)
        source.connects.+=:(backEdge)
        sink.connects.+=:(backEdge)
        return backEdge
    }

    //Todo.. add incomingedges to vertex class
    def getIncomingedges(v: Vertex): ListBuffer[Edge] = {
        return v.incomingEdges
    }

    def splitFreeVertices(): Unit = {
        for (v <- vertices) {
            if (v.outEdges.isEmpty && v.incomingEdges.isEmpty) {
                vertices -= v
                freeVertices += v
            }
        }
    }

    def initGraph(v: ListBuffer[Vertex], e: ListBuffer[Edge]): Unit = {
        vertices = v.clone()
        edges = e.clone()
    }

    //    def sortVertices(): Unit ={
    //      for(v <- vertices){
    //        v.connects = v.connects.sortBy(_.num)
    //      }
    //    }

    def removeEdge(e: Edge): Unit = {
        e.from.children -= e.to
        e.from.removeAdj(e)
        e.from.removeConnects(e)
        e.to.removeConnects(e)
        edges -= e
    }
}

class Edge(id: Int, v: Vertex, w: Vertex, typ: Boolean = false) {
    var eid = id
    var from = v
    var to = w
    var isVirtual = typ
    var isNotVisited = true
    var isHidden = false
    var isTreeEdge = false

    /*
    * get adjacency list of v1
    * */
    def getAnotherVertex(v1: Vertex): Vertex = {
        if (v1.equals(from))
            return to
        else if (v1.equals(to))
            return from
        else
            return null
    }

    /*
    * if this is return edge
    * */
    def isChangeDirection(v: Vertex, w: Vertex): Boolean = {
        if (this.from.equals(w)) {
            this.from = v
            this.to = w
            return true
        } else {
            return false
        }
    }

    def restoreDirection(v: Vertex, w: Vertex): Unit = {
        this.from = v
        this.to = w
    }

    override def equals(o: Any) = o match {
        case that: Edge =>
            if (isVirtual) {
                this.eid == that.eid
            } else
                (that.from.uuid == this.from.uuid && that.to.uuid == this.to.uuid && that.isVirtual.equals(this.isVirtual))
        case _ => false
    }

    override def hashCode: Int = v.uuid * 41 + w.uuid * 31

    override def toString: String = s"${from.name} -> ${to.name}, $isVirtual"
}


case class RPSTNode() {
    var entry: Vertex = null
    var exit: Vertex = null
    val vertices = new HashSet[Vertex]()
    var fragment: EdgeFragment = EdgeFragment()
    var ntype: NodeType = Undefined
    var tctnode: TCTreeNode = null
    val children = new ListBuffer[RPSTNode]()
    val uuid = java.util.UUID.randomUUID().hashCode()

    override def hashCode = uuid

    override def equals(o: Any) = o match {
        case that: RPSTNode => that.hashCode == this.hashCode
        case _ => false
    }

    def entryName: String = {
        if (entry != null) {
            return entry.name
        } else {
            return "Null"
        }
    }

    def exitName: String = {
        if (exit != null) {
            return exit.name
        } else {
            return "Null"
        }
    }

    //only sequence and bond
    def removeVertex(v: Vertex): Unit ={
        vertices -= v

        if(v.equals(entry)){
            for(e <- children){
                if(e.ntype.equals(Trivial) && e.vertices.contains(v)){
                    e.vertices -= v
                    entry = e.vertices.head
                    children -= e
                }
            }
        }
        if(v.equals(exit)){
            for(e <- children){
                if(e.ntype.equals(Trivial) && e.vertices.contains(v)){
                    e.vertices -= v
                    exit = e.vertices.head
                    children -= e
                }
            }
        }
        for(e <- children){
            if(e.ntype.equals(Trivial) && e.vertices.contains(v)){
                children -= e
            }
        }
        //todo fragment edges re-structure
    }

    override def toString: String = s"$ntype: ${entryName} -> ${exitName}"
}

case class EdgeFragment() {
    val edges = new HashSet[Edge]()
}

case class TCTree() {
    var root: TCTreeNode = null
    val treeNodes = new HashSet[TCTreeNode]()
    val adjNodes: Map[TCTreeNode, HashSet[TCTreeNode]] = Map()
    val verticesToEdges: Map[TCTreeNode, ListBuffer[TCTreeEdge]] = Map()
    val edges = new HashSet[TCTreeEdge]()
    var graph: RGraph = null
}

case class TCTreeNode() {
    var ntype: NodeType = Undefined
    var edges = new HashSet[Edge]()
    var parent: TCTreeNode = null
    val children = new HashSet[TCTreeNode]()
    var virtualEdges = new HashSet[Edge]()
    val uuid = java.util.UUID.randomUUID().hashCode()

    override def hashCode = uuid

    override def equals(o: Any) = o match {
        case that: TCTreeNode => that.hashCode == this.hashCode
        case _ => false
    }

    override def toString: String = ntype + ": " + edges.toString()
}

case class TCTreeEdge() {
    var source: TCTreeNode = _
    var target: TCTreeNode = _
    val uuid = java.util.UUID.randomUUID().hashCode()

    def setEdge(e1: TCTreeNode, e2: TCTreeNode): Unit = {
        source = e1
        target = e2
    }

    def getAnotherVertex(n: TCTreeNode): TCTreeNode = {
        if (n.equals(source)) {
            return target
        } else if (n.equals(target)) {
            return source
        } else {
            return null
        }
    }

    override def hashCode = uuid

    override def equals(o: Any) = o match {
        case that: TCTreeEdge => that.hashCode == this.hashCode
        case _ => false
    }

    override def toString: String = source + " -> " + target

}




case class ComponentWithVE(e: Edge, c: Component) {
    val virtualEdges = e
    val component = c
    var compType: NodeType = Undefined

    def setComptype(t: NodeType): Unit = {
        compType = t
    }
}

case class EdgeKey(e: Edge, t: NodeType) {

    override def equals(o: Any) = o match {
        case that: EdgeKey => ((that.e.equals(this.e)) && (that.t.equals(this.t)))
        case _ => false
    }

    private var s = 1
    t match {
        case Bond => s = 41
        case Polygon => s = 73
        case Rigid => s = 89
        case Trivial => s = 2
        case _ => s = 1
    }

    override def hashCode: Int = e.from.uuid * 17 + e.to.uuid * 79

}

case class BoundaryNode() {
    var num, incoming, outgoing = 0
}

case class Component() {
    val uuid = java.util.UUID.randomUUID().hashCode()
    val virtualEdges = new HashSet[Edge]()
    val edges = new HashSet[Edge]()
    val children = new ListBuffer[Component]()
    var compType: NodeType = _
    var father: Component = _

    def setComptype(t: NodeType): Unit = {
        compType = t
    }

    override def equals(o: Any) = o match {
        case that: Component => that.hashCode == this.hashCode
        case _ => false
    }

    override def hashCode = uuid

    override def toString: String = {

        return s"edges: $edges  ||  virtual: ${virtualEdges} "
    }
}

class Vertex(n: String) {
    val uuid = java.util.UUID.randomUUID().hashCode()
    val name = n
    var num, nd, father = 0
    var lowpt1, lowpt2, firstChild: Int = 0
    var flag: Boolean = true
    var visited: Boolean = false
    var vtype: String = ""
    val high = new ListBuffer[Vertex]()
    val children = new ListBuffer[Vertex]()
    var outEdges, incomingEdges = new ListBuffer[Edge]()
    var connects = new ListBuffer[Edge]()
    val adjList = new ListBuffer[Edge]()
    val adjList1 = new ListBuffer[Edge]()
    //    def copy(v: Vertex): Unit ={
    //      num = v.num
    //      lowpt1 = v.lowpt1
    //      lowpt2 = v.lowpt2
    //      high = v.high
    //      nd = v.nd
    //      father = v.father
    //      flag = v.flag
    //      visited = v.visited
    //      connects = v.connects
    //      adjList = v.adjList
    //      deg = v.deg
    //    }
    def degree(): Int = {
        connects.size
    }


    def addChild(v: Vertex): Unit = {
        children += v
    }

    def removeChild(v: Vertex): Unit ={
        children -= v
    }

    def addOutEdges(e: Edge): Unit = {
        outEdges += e
    }

    def removeOutEdges(e: Edge): Unit = {
        outEdges -= e
    }

    def addIncomingEdges(e: Edge): Unit = {
        incomingEdges += e
    }

    def removeIncomingEdges(e: Edge): Unit = {
        incomingEdges -= e
    }

    def addConnects(e: Edge): Unit = {
        connects += e
    }

    def removeConnects(e: Edge): Unit = {
        connects -= e
//        for (ce <- connects) {
//            if (isSameEdge(ce, e)) {
//                connects -= ce
//            }
//        }
    }
    def addAdj(e: Edge): Unit ={
        adjList += e
//        addChild(e.getAnotherVertex(this))
    }

    def addAdj1(e: Edge): Unit ={
        adjList1 += e
        //        addChild(e.getAnotherVertex(this))
    }

    def removeAdj(e: Edge): Unit = {
        adjList -= e
//        for (ae <- adjList) {
//            if (isSameEdge(ae, e)) {
//                adjList -= ae
//                removeChild(ae.getAnotherVertex(this))
//            }
//        }
    }

    def removeAdj1(e: Edge): Unit = {
        adjList1 -= e
        //        for (ae <- adjList) {
        //            if (isSameEdge(ae, e)) {
        //                adjList -= ae
        //                removeChild(ae.getAnotherVertex(this))
        //            }
        //        }
    }

    //    def changeFirstChild(v: Vertex): Unit ={
    //      if(firstchild.equals(v)){
    //        if(v.adjList.contains(v)){
    //          if(!firstchild.equals(v.adjList.last)){
    //            for(i <- v.adjList.indices){
    //              if(firstchild.equals(v.adjList(i))){
    //                firstchild = v.adjList(i+1).getAnotherVertex(v)
    //                return
    //              }
    //            }
    //          }else{
    //            firstchild = null
    //          }
    //        }
    //      }
    //      //      adjList -= v
    //    }

    def changeHigh(v: Vertex): Unit = {
        high -= v
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

    override def equals(o: Any) = o match {
        case that: Vertex => that.hashCode == this.hashCode
        case _ => false
    }

    override def hashCode = uuid

    override def toString: String = {
//        "num = " + num + "name = " + name + " father = " + father + " nd = " + nd
        name
    }
}

case class Fragment(){
    val uuid = java.util.UUID.randomUUID().hashCode()
    var v: Vertex = null
    var x, y = 0
    var content: RPSTNode = null
    val children,outEdges = ListBuffer[Fragment]()
    var nodeType: NodeType = null
    var entryNode, exitNode: Fragment = null
    var father: Fragment = null
    val vertices = ListBuffer[Vertex]()
    var isDrawed = false

    override def toString: String = {
        if(v != null && nodeType.equals(Trivial)){
            s"name: ${v.name}, type: $nodeType"
        }else{
            s"content: ${content.vertices.toString()}, type: $nodeType"
        }

    }

    override def equals(o: Any) = o match {
        case that: Fragment => that.hashCode == this.hashCode
        case _ => false
    }

    override def hashCode = uuid

}