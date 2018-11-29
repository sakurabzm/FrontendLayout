import scala.collection.mutable.{ListBuffer, Map, Set, Stack, HashMap}
import scala.math._
import scala.util.control.Breaks._
import TestGraph._

import scala.collection.mutable

object PalmTreeTest extends App{

  var i = 0
  sealed trait TreeType
  case object Arc extends TreeType
  case object Frond extends TreeType

  sealed  trait CompType
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

  //can change to SPQR tree, depends on demands
//  sealed trait ComponentsTree{
//    var children = new ListBuffer[ComponentsTreeBranch]()
//    var content: Component = null
//  }
  case class ComponentsTreeRoot(r: Vertex, s: Vertex){
    val root = r
    val sink = s
    var children = new ListBuffer[ComponentsTreeBranch]()

    def addChild(b: ComponentsTreeBranch): Unit ={
      children += b
    }
  }
  case class ComponentsTreeBranch(e: Edge, c: Component = null, t: CompType){
    val edge = e
    val ctype = t
    var entry, exit: Vertex = null
    var in1, in2, out1, out2 = false
    var children = new ListBuffer[ComponentsTreeBranch]()
    val content = c
    var isFragment = false
  }

  case class Edge(id: Int, from: Vertex, to: Vertex, typ: Boolean = false){
    var eid = id
    val v = from
    val w = to
    var isVirtual = typ


    override def equals(o: Any) = o match {
      case that: Edge =>
        if(isVirtual){
          if(this.eid == that.eid)
            true
          else
            false
        }else
          (that.from.num == this.from.num && that.to.num == this.to.num && that.typ.equals(this.typ))
      case _ => false
    }

    override def hashCode: Int = 41 * (41 * from.num + 59 * to.num + 41)

    override def toString: String = s"${v.name} -> ${w.name}"
  }

  case class ComponentWithVE(e: Set[Edge], c: Component){
    val virtualEdges = e
    val component = c
    var compType: CompType = null

    def setComptype(t: CompType): Unit ={
      compType = t
    }
  }

  case class EdgeKey(e: Edge, t: CompType){

    override def equals(o: Any) = o match {
      case that: EdgeKey => ((that.e.equals(this.e)) && (that.t.equals(this.t)))
      case _ => false
    }
    private var s = 1
    t match {
      case Bond => s = 41
      case Polygon => s = 73
      case Triconnected => s = 89
    }
    override def hashCode: Int = 31 * (41 * e.from.num + 59 * e.to.num + s)

  }

  case class Graph(){
    val id = 0
    var vertices = new ListBuffer[Vertex]()
    var edges = new ListBuffer[Edge]()
    var src: Vertex = null
    var snk: Vertex = null

    def createMateInfo(): Unit ={
      MetaInfo.addNewGraph(id)
    }
    def addVertex(v: Vertex): Vertex ={
      vertices += v
      return v
    }
    def addEdge(source: Vertex, sink: Vertex): Edge ={
      val eid = MetaInfo.getEdgeIdAndAutoIncrement
      val e = new Edge(eid, source, sink)
      edges += e
      source.transitions += sink
//      if(!source.connects.contains(sink))
        source.connects += sink
//      if(!sink.connects.contains(source))
        sink.connects += source
      return e
    }
    def addBackEdge(sink: Vertex, source: Vertex): Edge ={
      val eid = MetaInfo.getEdgeIdAndAutoIncrement
      src = source
      snk = sink
      if(!source.connects.contains(sink))
        source.connects += sink
      if(!sink.connects.contains(source))
        sink.connects += source

      val backEdge = new Edge(eid, snk, src)
      edges += backEdge
      return backEdge
    }

    //Todo.. add incomingedges to vertex class
    def getIncomingedges(v: Vertex): ListBuffer[Edge] ={
      val ies = new ListBuffer[Edge]()
      for(e <- edges){
        if(e.to.equals(v)){
          ies += e
        }
      }
      return ies
    }

    def initGraph(v: ListBuffer[Vertex], e: ListBuffer[Edge]): Unit ={
      vertices = v.clone()
      edges = e.clone()
    }
    def sortVertices(): Unit ={
      for(v <- vertices){
        v.connects = v.connects.sortBy(_.num)
      }
    }

    def removeEdge(e: Edge): Unit ={
      e.from.transitions -= e.to
      e.from.connects -= e.to
      e.to.connects -= e.from
      edges -= e
    }
  }

  case class BoundaryNode(){
    var num, incoming, outgoing = 0
  }

  case class Component(){
    var edges = new ListBuffer[Edge]()
    var boundary = (BoundaryNode, BoundaryNode)
    var entry = BoundaryNode
    var exit = BoundaryNode


    override def toString: String = {
      var s = ""
      for(e <- edges){
        s += s"${e.from.name} -> ${e.to.name}   "
      }
      return s
    }
  }

  case class Vertex(n: String){
    val uuid = java.util.UUID.randomUUID().hashCode()
    val name = n
    var num, lowpt1, lowpt2, nd, father, high, son = 0
    var firstchild: Vertex = null
    var flag: Boolean = true
    var visited: Boolean = false
    var ntype: String =""
    var transitions = new ListBuffer[Vertex]()
    var connects = new ListBuffer[Vertex]()
    var adjList = new ListBuffer[Vertex]()
    var deg = 0

    def copy(v: Vertex): Unit ={
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
    def degree(): Int ={
      return max(0,connects.length + deg)
    }

    def getFirstChild(): Vertex ={
      return firstchild
    }

    def addConnects(v: Vertex): Unit ={
      //      if(!connects.contains(v))
      connects += v
    }
    def removeConnects(v: Vertex): Unit ={
      connects -= v
    }

    def changeFirstChild(v: Vertex): Unit ={
      if(firstchild.equals(v)){
        if(v.adjList.contains(v)){
          if(!firstchild.equals(v.adjList.last)){
            for(i <- v.adjList.indices){
              if(firstchild.equals(v.adjList(i))){
                firstchild = v.adjList(i+1)
                return
              }
            }
          }else{
            firstchild = null
          }
        }
      }
      //      adjList -= v
    }

    def removeHigh(vn: Int): Unit ={
      high -= vn
    }

    override def equals(o: Any) = o match {
      case that: Vertex => that.hashCode == this.hashCode
      case _ => false
    }
    override def hashCode = uuid
    override def toString: String = {
//      "num = " + num + " father = " + father + " nd = " + nd + " lowpt1 = " + lowpt1 + " lowpt2 = " + lowpt2
      "name = " + name
    }
  }

  //init
//  type Edge = (Vertex, Vertex)
//  type VirtualEdge = (Edge, Component, CompType, Int)
  type SimpleVirtualEdge = (Edge,Component)
  val graph = TestGraph().getGraph()
  var normalizedGraph: Graph = new Graph
  var length = 20
  lazy val root: Vertex = normalizedGraph.src
  lazy val comptreeroot = ComponentsTreeRoot(normalizedGraph.src, normalizedGraph.snk)
  var virtualEdgeMap: Map[Edge,Component] = Map()
  var verticeslist = new ListBuffer[Vertex]()
  var bucket = new Array[ListBuffer[Edge]](3*length + 2)
  var palmtree = new ListBuffer[Edge]()
  var startPath = new ListBuffer[(Int, Int)]()
  var tStack = Stack[(Int, Vertex, Vertex)]()
  var eStack = Stack[Edge]()
  val eos = (0, new Vertex(""), new Vertex(""))
  var virtualEdgeList = new ListBuffer[ComponentWithVE]
  var newTreeEdgeList = new ListBuffer[Edge]
//  var triComponents: Map[EdgeKey, Component] = Map()
  var triComponents = new ListBuffer[ComponentWithVE]
  var search2num = 0
  var veid = 0
  val ov2nv: HashMap[Vertex,Vertex] = HashMap.empty[Vertex,Vertex]
  val ne2oe: Map[Edge, Edge] = Map()
  val extraEdges = new ListBuffer[Edge]
//  var triComponents = new ListBuffer[Component]


  for(i <- bucket.indices){
    bucket(i) = new ListBuffer[Edge]()
  }

  def search1(v: Vertex, u: Vertex): Unit ={
    i += 1
    v.nd = 0
    v.num = i
    v.lowpt1 = i
    v.lowpt2 = i
    for (w <- v.connects){
      if(w.num == 0){
        palmtree += newEdge(v, w)
        v.nd += 1
        w.father = v.num
        search1(w, v)
        v.nd += w.nd

        if(v.lowpt1 > w.lowpt1){
          v.lowpt2 = min(v.lowpt1, w.lowpt2)
          v.lowpt1 = w.lowpt1
        }else if(v.lowpt1 == w.lowpt1){
          v.lowpt2 = min(v.lowpt2, w.lowpt2)
        }else{
          v.lowpt2 = min(v.lowpt2, w.lowpt1)
        }

      }else if((v.num > w.num) && ((w.num != u.num) || (v.flag == false))){

        palmtree += newEdge(v, w)
        if(w.num < v.lowpt1){
          v.lowpt2 = v.lowpt1
          v.lowpt1 = w.num
        }else if(w.num > v.lowpt1){
          v.lowpt2 = min(v.lowpt2, w.num)
        }

      }else if(w.num == u.num && v.flag == true){
        v.flag = false
      }
    }
  }

  def isSameEdge(e1: Edge, e2: Edge): Boolean ={
    if((e1.from.num == e2.from.num && e1.to.num == e2.to.num) ||
       (e1.from.num == e2.to.num && e1.to.num == e2.from.num))
      return true
    else
      return false
  }

  def clearNum(): Unit ={
    for(v <- verticeslist){
      v.num = -1
    }
  }

  def search2(v: Vertex): Unit ={
    v.num = search2num - v.nd
    v.lowpt1 = v.num
    v.lowpt2 = v.num
    for(w <- v.adjList){
      if(w.num <= 0){
        search2(w)
      }
      if(v.son == 0){
        v.son = w.num
        search2num -= 1
        if(w.lowpt1 < v.lowpt1){
          v.lowpt2 = min(v.lowpt1, w.lowpt2)
          v.lowpt1 = w.lowpt1
        }else if(w.lowpt1 == v.lowpt1){
          v.lowpt2 = min(v.lowpt2, w.lowpt2)
        }else{
          v.lowpt2 = min(v.lowpt2, w.lowpt1)
        }
      }else{
        //todo change to vertex map
        val wfather: Vertex = verticeslist.find(p => p.num == w.father).get
        wfather.high = max(wfather.high, v.num)
        if(w.num < v.lowpt1){
          v.lowpt2 = v.lowpt1
          v.lowpt1 = w.num
        }else if(w.num > v.lowpt1){
          v.lowpt2 = min(v.lowpt2, w.num)
        }
      }
    }
  }

  def sort(): Unit = {
    for (edge <- palmtree) {
      if (edge.v.num > edge.w.num) {
        bucket((3 * edge.w.num) + 1) += newEdge(edge.v, edge.w)
        edge.w.high += edge.v.num
      }else if(edge.v.num > edge.w.lowpt2){
        bucket(3 * edge.w.lowpt1) += newEdge(edge.v, edge.w)
      }else{
        bucket(3 * edge.w.lowpt1 + 2) += newEdge(edge.v, edge.w)
      }
    }
    for(i <- bucket.indices){
      if(bucket(i).length > 0){
        for(edge <- bucket(i)){
          edge.v.adjList += edge.w
        }
      }
    }
    for(v <- normalizedGraph.vertices){
//      println("v " + v.name)
      v.firstchild = v.adjList.head
    }
  }

  var start = true
  def findStartPath(v: Vertex): Unit ={

    for(w <- v.adjList){
      if(v.num < w.num){
        if(start){
          startPath += ((v.num, w.num))
          start = false
        }
        findStartPath(w)
      }else{
        if(start){
          startPath += ((v.num, w.num))
        }else{
          start = true
        }
      }
    }
  }

  def pathSearch(v: Vertex): Unit ={
    var h = 0
    var a = new Vertex("")
    var b = new Vertex("")
    v.visited = true
    for (w <- v.adjList){
      if(tStack.isEmpty){
        h = 0
        a = new Vertex("")
        b = new Vertex("")
      }else{
        h = tStack.top._1
        a = tStack.top._2
        b = tStack.top._3
      }

      if(v.num < w.num) {
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
            tStack.push((w.num + w.nd - 1, normalizedGraph.vertices.find(p => p.num == w.lowpt1).get, v))
          }else{
            val bb = deltri.top._3
            val y = deltri.sortBy(_._1).last._1
            //            deltri.foreach(f => if (y < f._1) y = f._1)
            tStack.push((max(y, w.num + w.nd - 1), normalizedGraph.vertices.find(p => p.num == w.lowpt1).get, bb))
          }
          tStack.push(eos)
        }

        pathSearch(w)
        eStack.push(newEdge(v, w))
        /*
        * check type2 pairs
        */

        var wcopy = w
        while(v.num != 1 && ((a.num == v.num) || (wcopy.degree() == 2 && wcopy.getFirstChild().num > wcopy.num))){

          if(a.num == v.num && b.father == a.num){
            tStack.pop()
          }else{
            var eab: Edge = null
            var ee: SimpleVirtualEdge = null
            if(wcopy.degree() == 2 && wcopy.getFirstChild().num > wcopy.num){
              println("debug type2...1... " + getEstackxn() + " -> " + getEstackyn())
              var c = new Component
              c.edges += newComponent(eStack.pop())
              val x = eStack.top.w
              println("debug type2...1... " + getEstackxn() + " -> " + getEstackyn())
              c.edges += newComponent(eStack.pop())
              ee = newVirtualEdge(newEdge(v, x, true), c)
              x.father = v.num
              println(s"type 2.1 ${v.name} -> ${x.name}")
              if(getEstackx() == v.num && getEstacky() == b.num){
                eab = eStack.pop()
              }
            }else{
              tStack.pop()
              var c = new Component
              var x = getEstackx()
              var y = getEstacky()
              while((x <= h && x >= a.num) && (y <= h && y >= a.num)){
                if(x == a.num && y == b.num){
                  println("debug type2...2... " + getEstackxn() + " -> " + getEstackyn())
                  eab = eStack.pop()
                  x = getEstackx()
                  y = getEstacky()
                }else{
                  println("debug type2...3... " + getEstackxn() + " -> " + getEstackyn())
                  c.edges += newComponent(eStack.pop())
                  x = getEstackx()
                  y = getEstacky()
                }
              }
              ee = newVirtualEdge(newEdge(a, b, true), c)
              b.father = a.num

            }
            if(eab != null){
              var c = new Component
              c.edges += newComponent(eab)
              println("debug type2...4... " + eab.v.name + " -> " + eab.w.name + " type: " + eab.isVirtual)
              c.edges += newComponent((ee._1))
              println("debug type2...4... " + ee._1.v.name + " -> " + ee._1.w.name + " type: " + eab.isVirtual)
              ee = newVirtualEdge(newEdge(v, b, true), c)
            }
            eStack.push(ee._1)
            makeTreeEdge(newEdge(v, b))
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
        v.adjList.foreach(f => if(!f.visited) notvisited = true)
        if(wcopy.lowpt2 >= v.num && wcopy.lowpt1 < v.num && (v.father != 1 || notvisited)){
          var c = new Component
          var x = getEstackx()
          var y = getEstacky()
          while ((x >= wcopy.num && x <= wcopy.num + wcopy.nd) || (y >= wcopy.num && y <= wcopy.num + wcopy.nd)){
            println("debug type1...1... " + getEstackxn() + " -> " + getEstackyn() + " type: " + eStack.top.isVirtual)
            c.edges += newComponent(eStack.pop())
            x = getEstackx()
            y = getEstacky()
          }

          // v -> lowpt1 and lowpt1 -> v
          var lowpt1w = normalizedGraph.vertices.find(p => p.num == wcopy.lowpt1).get
          var ee: SimpleVirtualEdge = newVirtualEdge(newEdge(v, lowpt1w, true),c)
          println("type1.0 " + v.name + " -> " + lowpt1w.name)

          if((x == lowpt1w.num && y == v.num)||(x == v.num && y == lowpt1w.num)){
            var c = new Component
            println("debug type1...2... " + getEstackxn() + " -> " + getEstackyn())
            eStack.pop()
            c.edges += newComponent(newEdge(lowpt1w, v))
            println("debug type1...3... " + ee._1.v.name + " -> " + ee._1.w.name)
            c.edges += newComponent(newEdge(lowpt1w, v))
            ee = newVirtualEdge(newEdge(lowpt1w, v, true), c)
            println("type1.1 " + lowpt1w.name + " -> " + v.name)
          }

          if(lowpt1w.num != v.father){
            eStack.push(ee._1)
            makeTreeEdge(newEdge(lowpt1w, v))
          }else{
            val c = new Component
            println("debug type1...4... " + lowpt1w.name + " -> " + v.name)
            c.edges += newComponent(newEdge(lowpt1w, v))
            c.edges += newComponent(newEdge(lowpt1w, v))
            ee = newVirtualEdge(newEdge(lowpt1w, v, true), c)
            makeTreeEdge(newEdge(lowpt1w, v))
            println("type1.2 " + lowpt1w.name + " -> " + v.name)
          }
        }

        /*
        * type1 finished
        */

        if (startPath.contains((v.num, wcopy.num)) && tStack.nonEmpty) {
          while (!tStack.top.equals(eos)) {
            tStack.pop()
          }
          tStack.pop()
        }
        if(tStack.isEmpty){
          h = 0
          a = new Vertex("")
          b = new Vertex("")
        }
        while ((a.num != v.num) && (b.num != v.num) && (v.high > h) && tStack.nonEmpty) {
          tStack.pop()
          if(tStack.isEmpty){
            h = 0
            a = new Vertex("")
            b = new Vertex("")
          }else{
            h = tStack.top._1
            a = tStack.top._2
            b = tStack.top._3
          }
        }
      }else{
        val e = newEdge(w, v)
        if(startPath.contains((v.num, w.num))){
          var triDel = false
          var deltri = Stack[(Int, Vertex, Vertex)]()
          while(a.num > w.num){
            deltri.push(tStack.pop())
            h = tStack.top._1
            a = tStack.top._2
            b = tStack.top._3
            triDel = true
          }
          if(triDel == false){
            tStack.push((v.num, w, v))
          }else{
            val bb = deltri.top._3
            val y = deltri.maxBy(_._1)._1
            tStack.push((y, w, bb))
          }
        }
        if(w.num == v.father){
          var c = new Component
          println("debug frond..." + e.v.name + " -> " + e.w.name)
          c.edges += newComponent(newEdge(w, v))
          println("debug frond..." + w.name + " -> " + v.name)
          c.edges += newComponent(newEdge(w, v))
          val ee = newVirtualEdge(newEdge(w, v, true), c)
          makeTreeEdge(ee._1)
          println("frond " + w.name + " -> " + v.name)
        }else{
          eStack.push(e)
        }
      }
    }
  }

  def getEstackx(): Int ={
    if(eStack.isEmpty){
      return 0
    }else{
      return eStack.top.v.num
    }
  }

  def getEstackxn(): String ={
    if(eStack.isEmpty){
      return "empty"
    }else{
      return eStack.top.v.name
    }
  }

  def getEstacky(): Int ={
    if(eStack.isEmpty){
      return 0
    }else{
      return eStack.top.w.num
    }
  }

  def getEstackyn(): String ={
    if(eStack.isEmpty){
      return "empty"
    }else{
      return eStack.top.w.name
    }
  }

  def newComponent(e: Edge): Edge ={

//    graphc.removeEdge(e)
    e.v.changeFirstChild(e.w)
    e.v.removeConnects(e.w)
    e.v.removeHigh(e.w.num)
    e.v.removeConnects(e.w)

    return e
  }

  def newEdge(v: Vertex, w: Vertex, isVirtual: Boolean = false): Edge ={
    val eid = MetaInfo.getEdgeIdAndAutoIncrement
    return Edge(eid, v, w, isVirtual)
  }

  def newVirtualEdge(edge: Edge, c: Component): SimpleVirtualEdge ={

    val v = edge.v
    val w = edge.w
    veid += 1
    edge.eid = veid
    c.edges += edge

    val se = Set[Edge]()
    se += edge
    val ve = ComponentWithVE(se, c)
    if(c.edges.length > 3){
      ve.setComptype(Triconnected)
    }else{
      var isPolygon = false
      val e = c.edges.head
      for(i <- c.edges){
        if(!isSameEdge(e, i)){
          isPolygon = true
        }
      }
      if(isPolygon){
        ve.setComptype(Polygon)
      }else{
        ve.setComptype(Bond)
      }
    }

    virtualEdgeList += ve

    v.addConnects(w)
    w.addConnects(v)

    if(!v.adjList.contains(w)) {
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
      if(fch != null){
        if(fch.num < v.num){
          fchn = 3 * fch.num + 1
        }else if(fch.lowpt2 < v.num){
          fchn = 3 * fch.lowpt1
        }else{
          fchn = 3 * fch.lowpt1 + 2
        }
        if(bn < fchn){
          v.firstchild = w
        }
      }else{
        v.firstchild = w
      }
    }
    (edge, c)
  }

  def makeTreeEdge(e : Edge): Unit ={
    palmtree += e
    newTreeEdgeList += e
  }

  /*
  * TODO. need to change to linear time, need to split the tricomponents from sink node
  */
  def buildTriComp(): Unit ={
    var lastComponent = new Component
    var lastType : CompType = Bond
    while(eStack.nonEmpty){
      lastComponent.edges += eStack.pop()
    }

    if(lastComponent.edges.length > 3){
      lastType = Triconnected
    }else{
      var vn = 0
      var v1 = lastComponent.edges.head.v.num
      for(i <- lastComponent.edges){
        if(i.v.num == v1 || i.w.num == v1){
          vn += 1
        }
      }
      if(vn < 3 && lastComponent.edges.length == 3){
        lastType = Polygon
      }
    }

    for(i <- virtualEdgeList.indices){
      val vi = virtualEdgeList(i)
      updateTricomponents(vi.compType, vi.component)
    }

    updateTricomponents(lastType, lastComponent)
  }

  //merge same virtual edge component
  def updateTricomponents(t: CompType, c: Component): Unit ={
    val se = Set[Edge]()
    if(t.equals(Bond) || t.equals(Polygon)){
      for(e <- c.edges){
        if(e.isVirtual){
          se += e
          for(tc <- triComponents){
            for(ve <- tc.virtualEdges){
              if(tc.compType.equals(t) && ve.eid == e.eid){
                tc.component.edges ++= c.edges
                tc.component.edges -= e
                tc.component.edges -= e
                se.clear()
                c.edges.clear()
                for(edge <- tc.component.edges){
                  if(edge.isVirtual){
                    se += edge
                  }
                }
                triComponents -= tc
                val nve = ComponentWithVE(se, tc.component)
                nve.setComptype(t)
                triComponents += nve
                return
              }
            }
          }
        }
      }
    }else{
      for(e <- c.edges){
        if(e.isVirtual){
          se += e
        }
      }
    }
    val nve = ComponentWithVE(se, c)
    nve.setComptype(t)
    triComponents += nve

  }

  def buildComponetsTreeRoot(): Unit ={
    var backedge: Edge = null
    var c: Component = null
    var t: CompType = null
    for(ve <- triComponents){
        for(e <- ve.component.edges){
          if(isSameEdge(e, newEdge(comptreeroot.r, comptreeroot.s))){
            e.isVirtual = true
            backedge = e
            c = ve.component
            t = ve.compType
            triComponents -= ve
            val b = new ComponentsTreeBranch(e, c, t)
            comptreeroot.addChild(b)
            return
          }
        }
    }
//    for(e <- c.edges){
//      if(e.isVirtual && !isSameEdge(e, backedge)){
//        val b = new ComponentsTreeBranch(e, c, t)
//        comptreeroot.addChild(b)
//      }
//    }
  }

  def buildComponentsTree(r: ComponentsTreeRoot): Unit ={
    for(b <- r.children){
      buildComponentsTreeBranch(b)
    }
  }

  def buildComponentsTreeBranch(b: ComponentsTreeBranch): Unit ={
    for(tc <- triComponents){
      for(ve <- tc.virtualEdges){
        if(b.content != null && b.content.edges.nonEmpty && b.content.edges.contains(ve)){
          val c = ComponentsTreeBranch(ve, tc.component, tc.compType)
          triComponents -= tc
          b.children += c
          buildComponentsTreeBranch(c)
        }
      }
    }
  }

  /*
  * after bulid tricomponents
  */

  def normalizeGraph(g: Graph): Unit ={
    val sources = new ListBuffer[Vertex]
    val sinks = new ListBuffer[Vertex]
    val mixed = new ListBuffer[Vertex]

    //copy vertices
    for(v <- g.vertices){
      if(g.getIncomingedges(v).isEmpty){
        sources += v
      }
      if(v.transitions.isEmpty){
        sinks += v
      }
      if(g.getIncomingedges(v).size>1 && v.transitions.size>1){
        mixed += v
        println(s"mixed: ${v.name}")
      }
      ov2nv += v -> normalizedGraph.addVertex(new Vertex(v.name))
    }

    //copy edges
    for(e <- g.edges){
      ne2oe += (normalizedGraph.addEdge(ov2nv(e.from), ov2nv(e.to)) -> e)
    }

    //introduce single source
    val nsrc = new Vertex("NSRC")
    normalizedGraph.addVertex(nsrc)
    for(v <- sources){
      extraEdges += normalizedGraph.addEdge(nsrc, ov2nv(v))
    }

    //introduce single sink
    val nsnk = new Vertex("NSNK")
    normalizedGraph.addVertex(nsnk)
    for (v <- sinks) {
      extraEdges += (normalizedGraph.addEdge(ov2nv(v), nsnk))
    }

    //split mixed 'gateways', i.e., vertices with multiple inputs and outputs
    for(v <- mixed){
      val nv = new Vertex(s"*${v.name}*")
      normalizedGraph.addVertex(nv)

      for(ie <- normalizedGraph.getIncomingedges(ov2nv(v))){
        normalizedGraph.removeEdge(ie)
        val e: Edge = ne2oe.remove(ie).get
        val ne = normalizedGraph.addEdge(ov2nv(e.from), nv)
        ne2oe += ne -> e
      }

      extraEdges += normalizedGraph.addEdge(nv, ov2nv(v))
    }

    val backEdge = normalizedGraph.addBackEdge(nsnk, nsrc)
    extraEdges += backEdge
  }

  def constructRPST(): Unit ={

  }


  def findBoundaryNodes(c: ComponentsTreeBranch): Unit ={
    val vertexList = mutable.Set[Vertex]()
    var in,out = 0
    val v1 = c.e.from
    val v2 = c.e.to
    var entry, exit: Vertex = null
    var order = false
    for(e <- c.content.edges){
      if(e.from.equals(v1)) c.out1 = true
      if(e.from.equals(v2)) c.out2 = true
      if(e.to.equals(v1)) c.in1 = true
      if(e.to.equals(v2)) c.in2 = true
    }
    if(c.children.nonEmpty) for(child <- c.children){
      findBoundaryNodes(child)
    }
    //compute in and out

    for(child <- c.children){
      val cv1 = child.e.from
      val cv2 = child.e.to
      var cin1, cin2, cout1, cout2 = false
      for(e <- c.content.edges){
        if(!e.equals(child.e)){
          if(e.from.equals(cv1)) cin1 = true
          if(e.from.equals(cv2)) cin2 = true
          if(e.to.equals(cv1)) cout1 = true
          if(e.to.equals(cv2)) cout2 = true
        }
      }
      if((child.out1 && !cout1) || !child.in1) child.entry = child.e.from
      if((child.in1 && !cin1) || !child.out1) child.exit = child.e.from
      if((child.out2 && !cout2) || !child.in2) child.entry = child.e.to
      if((child.in2 && !cin2) || !child.out2) child.exit = child.e.to
      if(child.entry != null && child.exit != null){
        child.isFragment = true
        order = true
        // Todo If a child component is a fragment, order the child components from entry to exit.
        if(child.ctype.equals(Bond)){

        }
      }
    }

    if(order){

    }
  }
  /*
   *Step 3. Restructure the tree of the triconnected components into the tree of the
   *canonical fragments (the RPST).
   */




  var superroot = new Vertex("superroot")
  superroot.num = -1
  normalizeGraph(graph)
  search1(normalizedGraph.src, superroot)
  sort()
  search2num = normalizedGraph.vertices.size
  clearNum()
  search2(normalizedGraph.src)
  findStartPath(normalizedGraph.src)
//  graphc.initGraph(verticeslist, palmtree)
  tStack.push(eos)
  pathSearch(normalizedGraph.src)


  println("--------components----------")
  virtualEdgeList.foreach(f => {
    println(s"v: ${f.virtualEdges.head.v.name} -> w: ${f.virtualEdges.head.w.name}")
    println("componets:")
    println(f.component)
    println("*************************")
  })

  buildTriComp()

  println("--------Tri Components----------")
  for(ve <- triComponents){
    println("tricomponets:")
    println(s"edge: ${ve.virtualEdges.head.from.name} -> ${ve.virtualEdges.head.to.name}")
    println()
    println(ve.component.toString)
    println("------------------------------------")
  }

  println("**********************************************")



  buildComponetsTreeRoot()
  buildComponentsTree(comptreeroot)

  //  println("--------transions----------")
  //  verticeslist.foreach(f => {
  //    println("v: " + f.num)
  //    println(f.transition.foreach(g => {
  //      print(g.num + "   ")
  //    }))
  //    println
  //  })
  //  println("--------------------------------------")




  println("----------------Components Tree----------------------")
  outputTreeRoot(comptreeroot)
  def outputTreeRoot(r: ComponentsTreeRoot): Unit ={
    if(r.children.nonEmpty){
      println(s"root: ${r.root.name}   ")
      for(c <- r.children){
        outputTreeBranch(c)
      }
    }
  }

  def outputTreeBranch(b: ComponentsTreeBranch): Unit ={
    if(b.children.isEmpty){
      println("------------------branch---------------------------")
      println(b.e.toString)
      println()
      println(b.content.toString)
    }else{
      for(c <- b.children){
        outputTreeBranch(c)
      }
      println("************************************************")
      println(b.e)
      println()
      println(b.content.toString)
    }
  }

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
