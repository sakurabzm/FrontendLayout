import scala.collection.mutable.{HashSet, ListBuffer, Map}
import scala.math.max

//class GraphMiscellaneous {
//
//}
sealed trait TreeType

case object Arc extends TreeType

case object Frond extends TreeType

sealed trait NodeType

case object Trivial extends NodeType

case object Bond extends NodeType

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
    var edges = new ListBuffer[GEdge]()
    var src: Vertex = null
    var snk: Vertex = null

    def createMateInfo(): Unit = {
        MetaInfo.addNewGraph(id)
    }

    def addVertex(v: Vertex): Vertex = {
        vertices += v
        return v
    }

    def addEdge(source: Vertex, sink: Vertex): GEdge = {
        val eid = MetaInfo.getEdgeIdAndAutoIncrement
        val e = new GEdge(eid, source, sink)
        edges += e
        source.transitions += sink
        //      if(!source.connects.contains(sink))
        source.connects += e
        //      if(!sink.connects.contains(source))
        sink.connects += e
        return e
    }

    def addEdgeToHead(source: Vertex, sink: Vertex): GEdge = {
        val eid = MetaInfo.getEdgeIdAndAutoIncrement
        val e = new GEdge(eid, source, sink)
        edges += e
        source.connects.+=:(e)
        sink.connects.+=:(e)
        return e
    }

    def addBackEdge(sink: Vertex, source: Vertex): GEdge = {
        val eid = MetaInfo.getEdgeIdAndAutoIncrement
        src = source
        snk = sink

        val backEdge = new GEdge(eid, snk, src)
        edges += backEdge
        source.connects.+=:(backEdge)
        sink.connects.+=:(backEdge)
        return backEdge
    }

    //Todo.. add incomingedges to vertex class
    def getIncomingedges(v: Vertex): ListBuffer[GEdge] = {
        val ies = new ListBuffer[GEdge]()
        for (e <- edges) {
            if (e.to.equals(v)) {
                ies += e
            }
        }
        return ies
    }

    def initGraph(v: ListBuffer[Vertex], e: ListBuffer[GEdge]): Unit = {
        vertices = v.clone()
        edges = e.clone()
    }

    //    def sortVertices(): Unit ={
    //      for(v <- vertices){
    //        v.connects = v.connects.sortBy(_.num)
    //      }
    //    }

    def removeEdge(e: GEdge): Unit = {
        e.from.transitions -= e.to
        e.from.connects -= e
        e.to.connects -= e
        edges -= e
    }
}

case class GEdge(id: Int, from: Vertex, to: Vertex, typ: Boolean = false) {
    var eid = id
    var v = from
    var w = to
    var isVirtual = typ
    var isNotVisited = true
    var isHidden = false
    var isTreeEdge = false

    def getAnotherVertex(v1: Vertex): Vertex = {
        if (v1.equals(v))
            return w
        else if (v1.equals(w))
            return v
        else
            return null
    }

    def changeDirection(v: Vertex, w: Vertex): Boolean = {
        if (this.v.equals(w)) {
            this.v = v
            this.w = w
            return true
        } else {
            return false
        }
    }

    def restoreDirection(v: Vertex, w: Vertex): Unit = {
        this.v = v
        this.w = w
    }

    override def equals(o: Any) = o match {
        case that: GEdge =>
            if (isVirtual) {
                this.eid == that.eid
            } else
                (that.from.uuid == this.from.uuid && that.to.uuid == this.to.uuid && that.typ.equals(this.typ))
        case _ => false
    }

    override def hashCode: Int = from.uuid * 41 + to.uuid * 31

    override def toString: String = s"${v.name} -> ${w.name}, $isVirtual"
}

sealed trait FragmentType
case object Atomic extends FragmentType
case object Sequence extends FragmentType
case object Branch extends FragmentType
case object Loop extends FragmentType
case object Unstructured extends FragmentType

case class Fragment(){
    var v: Vertex = null
    var x, y = 0
    var content: RPSTNode = null
    val children = ListBuffer[Fragment]()
    var nodeType: NodeType = null

    override def toString: String = {
        if(v != null && nodeType.equals(Trivial)){
            s"name: ${v.name}, type: $nodeType"
        }else{
            s"content: ${content.vertices.toString()}, type: $nodeType"
        }

    }

}

case class RPSTNode() {
    var entry: Vertex = null
    var exit: Vertex = null
    var fragment: EdgeFragment = EdgeFragment()
    val vertices = new HashSet[Vertex]()
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
    val edges = new HashSet[GEdge]()
}

case class TCTree() {
    var root: TCTreeNode = null
    val vertices = new HashSet[TCTreeNode]()
    val adjMap: Map[TCTreeNode, HashSet[TCTreeNode]] = Map()
    val verticesToEdges: Map[TCTreeNode, ListBuffer[TCTreeEdge]] = Map()
    val edges = new HashSet[TCTreeEdge]()
    var graph: RGraph = null
}

case class TCTreeNode() {
    var ntype: NodeType = Undefined
    var edges = new HashSet[GEdge]()
    var parent: TCTreeNode = null
    val children = new HashSet[TCTreeNode]()
    var virtualEdges = new HashSet[GEdge]()
    val uuid = java.util.UUID.randomUUID().hashCode()

    override def hashCode = uuid

    override def equals(o: Any) = o match {
        case that: TCTreeNode => that.hashCode == this.hashCode
        case _ => false
    }

    override def toString: String = ntype + ": " + edges.toString()
}

case class TCTreeEdge() {
    var source: TCTreeNode = null
    var target: TCTreeNode = null
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


case class ComponentWithVE(e: GEdge, c: Component) {
    val virtualEdges = e
    val component = c
    var compType: NodeType = Undefined

    def setComptype(t: NodeType): Unit = {
        compType = t
    }
}

case class EdgeKey(e: GEdge, t: NodeType) {

    override def equals(o: Any) = o match {
        case that: EdgeKey => ((that.e.equals(this.e)) && (that.t.equals(this.t)))
        case _ => false
    }

    private var s = 1
    t match {
        case Bond => s = 41
        case Polygon => s = 73
        case Rigid => s = 89
        case Trivial => s = 23
    }

    override def hashCode: Int = e.from.uuid * 17 + e.to.uuid * 79

}

case class BoundaryNode() {
    var num, incoming, outgoing = 0
}

case class Component() {
    val uuid = java.util.UUID.randomUUID().hashCode()
    var virtualEdges = new HashSet[GEdge]()
    var edges = new HashSet[GEdge]()
    var compType: NodeType = null

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

case class Vertex(n: String) {
    val uuid = java.util.UUID.randomUUID().hashCode()
    val name = n
    var num, nd, father = 0
    var firstChild: Vertex = null
    var lowpt1: Vertex = null
    var lowpt2: Vertex = null
    var flag: Boolean = true
    var visited: Boolean = false
    var ntype: String = ""
    val high = new ListBuffer[Vertex]()
    val transitions = new ListBuffer[Vertex]()
    var connects = new ListBuffer[GEdge]()
    val adjList = new ListBuffer[GEdge]()

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
        var degree = new HashSet[Vertex]()
        for (e <- connects) {
            degree += e.getAnotherVertex(this)
        }
        return degree.size
    }

    def calFirstChild(): Unit = {
        if (adjList.nonEmpty)
            firstChild = adjList.head.getAnotherVertex(this)
        else
            firstChild = null
    }

    def getFirstChild(): Vertex = {
        if (adjList.nonEmpty) {
            firstChild = adjList.head.getAnotherVertex(this)
            return firstChild
        } else {
            return null
        }
    }

    def addConnects(e: GEdge): Unit = {
        connects += e
    }

    def removeAdjs(e: GEdge): Unit = {
        for (ae <- adjList) {
            if (isSameEdge(ae, e)) {
                adjList -= ae
            }
        }
    }

    def removeConnects(e: GEdge): Unit = {
        for (ce <- connects) {
            if (isSameEdge(ce, e)) {
                connects -= ce
            }
        }
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

    def removeHigh(v: Vertex): Unit = {
        high -= v
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

    override def equals(o: Any) = o match {
        case that: Vertex => that.hashCode == this.hashCode
        case _ => false
    }

    override def hashCode = uuid

    override def toString: String = {
        //      "num = " + num + " father = " + father + " nd = " + nd + " lowpt1 = " + lowpt1 + " lowpt2 = " + lowpt2
        name
    }
}
