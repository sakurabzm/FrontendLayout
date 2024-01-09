package de.tkip.sbpm.frontend.Data

import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.generic.JsonCodec

import scala.collection.mutable.{ListBuffer, LinkedHashMap, Map, Set}
import com.flowtick.graphs.{Edge, Graph}
import com.flowtick.graphs.defaults._
import com.flowtick.graphs.graphml._
import de.tkip.sbpm.frontend.graph.GraphObject
import de.tkip.sbpm.frontend.graph.GraphObject._

import scala.collection.mutable

object Transform {

    @JsonCodec sealed trait JsonData

    //  case class ProcessJsonData(id: Int, name: Int, description: String, subjectMap)
    case class SavedJsonData(data: ListBuffer[ProcessJsonData]) extends JsonData
//    case class SavedJsonData1(data: ListBuffer[ProcessJsonData]) extends JsonData

//    case class ProcessJsonData(id: String, author: String, name: String, descriptrion: String,
//                               subjectMap: ListMap[String, SubjectJsonData]) extends JsonData

    case class ProcessJsonData(id: String, author: String, name: String, descriptrion: String,
                               subjectMap: LinkedHashMap[String, SubjectJsonData]) extends JsonData

//    case class SubjectJsonData(id: Int, name: String, startSubject: Boolean, subjectType: String,
//                               inputPool: Int, num: Int, description: String, x: Int, y: Int,
//                               stateMap: Map[Int, StateJsonData], arrowMap: Map[Int, TransitionJsonData],
//                               isMultiSubject: Boolean, callMacroMap: Map[Int, ListBuffer[StateJsonData]],
//                               startStateList: ListBuffer[Int]) extends JsonData

    case class SubjectJsonData(id: Int, name: String, startSubject: Boolean, subjectType: String,
                               inputPool: Int, num: Int, description: String, x: Int, y: Int,
                               stateMap: Map[Int, StateJsonData], arrowMap: Map[Int, TransitionJsonData],
                               isMultiSubject: Boolean, callMacroMap: Map[Int, ListBuffer[StateJsonData]],
                               startStateList: ListBuffer[Int]) extends JsonData

    case class TransitionJsonData(id: Int, transitionType: String, priority: Int, description: String,
                                  source: Int, target: Int, timeout: Int, activatedStateID: Int,
                                  x1: Int, x2: Int, y1: Int, y2: Int, x: Int, y: Int, arrowType: String,
                                  relatedSubjectName: String, relatedMessageType: String) extends JsonData

//    case class StateJsonData(id: Int, name: String, stateType: String, isStartState: Boolean, priority: Int,
//                             description: String, deepth: Int,
//                             parentID: Int, x: Int, y: Int, directChildrenTransitionsList: ListBuffer[TransitionJsonData],
//                             nonDirectChildrenTransitionsList: ListBuffer[TransitionJsonData]) extends JsonData

    case class StateJsonData(id: Int, name: String, stateType: String, isStartState: Boolean, priority: Int,
                             description: String, deepth: Int,
                             parentID: Int, x: Int, y: Int, directChildrenTransitionsMap: LinkedHashMap[Int, ListBuffer[TransitionJsonData]],
                             nonDirectChildrenTransitionsMap: LinkedHashMap[Int, ListBuffer[TransitionJsonData]]) extends JsonData

    implicit val jsonDataDecoder: Decoder[SavedJsonData] = deriveDecoder[SavedJsonData]
    implicit val jsonDataEncoder: Encoder[SavedJsonData] = deriveEncoder[SavedJsonData]
//    implicit val jsonDataDecoder1: Decoder[SavedJsonData1] = deriveDecoder[SavedJsonData1]
//    implicit val jsonDataEncoder1: Encoder[SavedJsonData1] = deriveEncoder[SavedJsonData1]
    //  def graphToJson(sl: ListBuffer[StateData]): String ={
    //    val lsj = ListBuffer[StateJsonData]()
    //    for(sd <- sl){
    //      val dctlj = ListBuffer[TransitionJsonData]()
    //      for(td <- sd.directChildrenTransitionsList){
    //        val tj = TransitionJsonData(td.id, td.transitionType, td.priority, td.description, td.source, td.target,
    //          td.timeout, td.activatedStateID)
    //        dctlj += tj
    //      }
    //      val ndctlj = ListBuffer[TransitionJsonData]()
    //      for(td <- sd.nonDirectChildrenTransitionsList){
    //        val tj = TransitionJsonData(td.id, td.transitionType, td.priority, td.description, td.source, td.target,
    //          td.timeout, td.activatedStateID)
    //        ndctlj += tj
    //      }
    //      val sj = StateJsonData(sd.id, sd.stateName, sd.stateType, sd.isStartState, sd.priority, sd.description,dctlj, ndctlj)
    //      lsj += sj
    //    }
    //    println("json: " + lsj.asJson.noSpaces)
    //    lsj.asJson.noSpaces
    //  }

    def saveProcess(pm: Map[String, ProcessData]): String = {
        val pjdl = ListBuffer[ProcessJsonData]()
        for (pd <- pm.valuesIterator) {
            var subjectMap: LinkedHashMap[String, SubjectJsonData] = LinkedHashMap()
            for (sub <- pd.subjectList) {
                var stateMap: Map[Int, StateJsonData] = Map()
                var arrowMap: Map[Int, TransitionJsonData] = Map()
                var callMacroMap: Map[Int, ListBuffer[StateJsonData]] = Map()
                for (st <- sub.stateList) {
                    val directChildrenTransitionsMap: LinkedHashMap[Int, ListBuffer[TransitionJsonData]] = LinkedHashMap()
                    for (tdl <- st.data.directChildrenTransitionsMap.values) {
                        var a: Arrow = null
                        for(td <- tdl){
                            if(sub.arrowMap.contains(td.id))
                                a = sub.arrowMap(td.id)
                        }
                        for(td <- tdl){
                            var tj: TransitionJsonData = null
                            if(st.data.stateType == "Send" || st.data.stateType == "Receive"){
                                tj = TransitionJsonData(td.id, td.transitionType, td.priority, td.label, td.source, td.target,
                                    td.timeout, td.activatedStateID, a.x1, a.x2, a.y1, a.y2, a.spacex, a.spacey, a.aType,
                                    td.information.relatedSubjectName, td.information.relatedMessageType)
                            }else{
                                tj = TransitionJsonData(td.id, td.transitionType, td.priority, td.label, td.source, td.target,
                                    td.timeout, td.activatedStateID, a.x1, a.x2, a.y1, a.y2, a.spacex, a.spacey, a.aType, "", "")
                            }
                            if(!directChildrenTransitionsMap.contains(td.target)){
                                val tl: ListBuffer[TransitionJsonData] = ListBuffer()
                                tl += tj
                                directChildrenTransitionsMap += td.target -> tl
                            }else{
                                directChildrenTransitionsMap(td.target) += tj
                            }
                            if(!arrowMap.contains(tdl.head.id)){
                                arrowMap += (tdl.head.id -> tj)
                            }
                        }

                    }
                    val nonDirectChildrenTransitionsMap: LinkedHashMap[Int, ListBuffer[TransitionJsonData]] = LinkedHashMap()
                    for (tdl <- st.data.nonDirectChildrenTransitionsMap.values) {
                        var a: Arrow = null
                        for(td <- tdl){
                            if(sub.arrowMap.contains(td.id))
                                a = sub.arrowMap(td.id)
                        }
                        for(td <- tdl){
                            var tj: TransitionJsonData = null
                            if(st.data.stateType == "Send" || st.data.stateType == "Receive"){
                                tj = TransitionJsonData(td.id, td.transitionType, td.priority, td.label, td.source, td.target,
                                    td.timeout, td.activatedStateID, a.x1, a.x2, a.y1, a.y2, a.spacex, a.spacey, a.aType,
                                    td.information.relatedSubjectName, td.information.relatedMessageType)
                            }else{
                                tj = TransitionJsonData(td.id, td.transitionType, td.priority, td.label, td.source, td.target,
                                    td.timeout, td.activatedStateID, a.x1, a.x2, a.y1, a.y2, a.spacex, a.spacey, a.aType, "", "" )
                            }
                            if(!nonDirectChildrenTransitionsMap.contains(td.target)){
                                val tl: ListBuffer[TransitionJsonData] = ListBuffer()
                                tl += tj
                                nonDirectChildrenTransitionsMap += td.target -> tl
                            }else{
                                nonDirectChildrenTransitionsMap(td.target) += tj
                            }
                            if(!arrowMap.contains(tdl.head.id)){
                                arrowMap += (tdl.head.id -> tj)
                            }
                        }
                    }
                    val sj = StateJsonData(st.data.id, st.data.stateName, st.data.stateType, st.data.isStartState, st.data.priority,
                        st.data.description, st.deep, st.parentID, st.sx, st.sy, directChildrenTransitionsMap, nonDirectChildrenTransitionsMap)
                    stateMap += (st.data.id -> sj)
                }
                val subjectJsonData = SubjectJsonData(sub.id, sub.name, sub.startSubject, sub.subjectType, sub.inputPool, sub.num,
                    sub.description, sub.subjectX, sub.subjectY, stateMap, arrowMap, sub.isMultiSubject, callMacroMap, sub.startStateList)
                subjectMap += (sub.id.toString -> subjectJsonData)
            }
            val pjd = ProcessJsonData(pd.id, pd.authorOfProcess, pd.nameOfProcess, pd.descriptionOfProcess, subjectMap)
            pjdl += pjd
        }
        val sjd = SavedJsonData(pjdl)
        val json = jsonDataEncoder(sjd)
        println("save json")
        json.noSpaces

    }

    //  def jsonToGraph(js: String): ListBuffer[StateData] ={
    ////    val stateDataList
    //    val json = parse(js).right.get
    //    val decodejson = jsonDataDecoder.decodeJson(json)
    //    val lsj = decodejson.right.get.asInstanceOf[ListBuffer[StateJsonData]]
    //  }
    def jsonToProcess(jsonString: String): Map[String, ProcessData] = {
        val json = parse(jsonString).right.get
        val jd = jsonDataDecoder.decodeJson(json).right.get
        val processMap: Map[String, ProcessData] = Map()
        val arrowsFromSourceMap: Map[Int, ListBuffer[Int]] = Map()
        for(pd <- jd.data){
            val processData = new ProcessData(pd.id, pd.author, pd.name, pd.descriptrion)
            for(sub <- pd.subjectMap.valuesIterator){
                val subjectData = new SubjectData(sub.id, sub.name, sub.startSubject, sub.subjectType, sub.inputPool,
                    sub.num, sub.description)
                subjectData.isMultiSubject = sub.isMultiSubject
                subjectData.startStateList ++= sub.startStateList
                subjectData.subjectX = sub.x
                subjectData.subjectY = sub.y
                for(state <- sub.stateMap.valuesIterator){
                    val stateData = StateData(state.id)
                    stateData.init(state.name, state.stateType, state.isStartState, state.priority, state.description)
                    val arrowsToTarget: ListBuffer[Int] = ListBuffer()
                    val childrenList: ListBuffer[Int] = ListBuffer()
                    val descendantList: ListBuffer[Int] = ListBuffer()
//                    println(s"看看map顺序: process: ${processData.nameOfProcess}, subject: ${subjectData.name}, " +
//                            s"state: ${stateData.stateName}, childMap: ${state.directChildrenTransitionsMap}")
                    for(value <- state.directChildrenTransitionsMap.values){
                        val directChildrenTransitionsList: ListBuffer[TransitionData] = ListBuffer()
                        for(tr <- value){
                            val transitionData = TransitionData(tr.id, tr.source, tr.target)
                            transitionData.init(tr.transitionType, tr.priority, tr.description, tr.timeout)
                            if(stateData.stateType == "Send" || stateData.stateType == "Receive"){
                                transitionData.information.setRelatedSubjectName(tr.relatedSubjectName)
                                transitionData.information.setRelatedMessageType(tr.relatedMessageType)
                                transitionData.information.setActionType(stateData.stateType)
                            }
                            directChildrenTransitionsList += transitionData
                            arrowsToTarget += tr.id
                            addIdToArrowsFromSourceMap(tr.id, tr.target, arrowsFromSourceMap)
                        }
                        if(directChildrenTransitionsList.nonEmpty){
                            val tr = value.head
                            val arrow = new Arrow(pd.id, sub.id.toString, directChildrenTransitionsList.head, tr.x1, tr.y1, tr.x2, tr.y2, tr.arrowType)
                            subjectData.arrowMap += (tr.id -> arrow)
                            stateData.directChildrenTransitionsMap += (directChildrenTransitionsList.head.target -> directChildrenTransitionsList)
                            childrenList += tr.target
                        }
                    }
                    for(value <- state.nonDirectChildrenTransitionsMap.values){
                        val nonDirectChildrenTransitionsList: ListBuffer[TransitionData] = ListBuffer()
                        for(tr <- value){
                            val transitionData = TransitionData(tr.id, tr.source, tr.target)
                            transitionData.init(tr.transitionType, tr.priority, tr.description, tr.timeout)
                            if(stateData.stateType == "Send" || stateData.stateType == "Receive"){
                                transitionData.information.setRelatedSubjectName(tr.relatedSubjectName)
                                transitionData.information.setRelatedMessageType(tr.relatedMessageType)
                                transitionData.information.setActionType(stateData.stateType)
                            }
                            nonDirectChildrenTransitionsList += transitionData
                            arrowsToTarget += tr.id
                            addIdToArrowsFromSourceMap(tr.id, tr.target, arrowsFromSourceMap)
                        }
                        if(nonDirectChildrenTransitionsList.nonEmpty){
                            val tr = value.head
                            val arrow = new Arrow(pd.id, sub.id.toString, nonDirectChildrenTransitionsList.head, tr.x1, tr.y1, tr.x2, tr.y2, tr.arrowType)
                            subjectData.arrowMap += (tr.id -> arrow)
                            stateData.nonDirectChildrenTransitionsMap += (nonDirectChildrenTransitionsList.head.target -> nonDirectChildrenTransitionsList)
                            descendantList += tr.target
                        }
                    }
                    val stateGraph = GraphObject.createGraph(state.stateType, stateData, pd.id, sub.id.toString)
                    stateGraph.setCoordinate(state.x, state.y)
                    stateGraph.arrowsToTarget ++= arrowsToTarget
                    stateGraph.childrenList ++= childrenList
                    stateGraph.descendantList ++= descendantList
                    stateGraph.setParent(state.parentID)
                    stateGraph.setDeep(state.deepth)
                    subjectData.addState(stateGraph)
                }
                for(state <- subjectData.stateList){
                    if(arrowsFromSourceMap.contains(state.data.id)){
                        state.arrowsFromSource ++= arrowsFromSourceMap(state.data.id)
                    }
                }
                processData.addSubject(subjectData)
            }
            val tl = processData.subjectList.reverse
            processData.subjectList.clear()
            processData.subjectList ++= tl
            processMap += (processData.id -> processData)
        }
        processMap
    }

    def addIdToArrowsFromSourceMap(aid: Int, tid: Int, map: Map[Int, ListBuffer[Int]]): Unit ={
        if(map.contains(tid)){
            map(tid) += aid
        }else{
            val li = ListBuffer[Int]()
            li += aid
            map += (tid -> li)
        }
    }

    def graphToGraphml(pd: ProcessData): String = {
        var subid2gsid: Map[String, String] = Map()
        var stid2gstid: Map[String, String] = Map()
        var si = 0
        var graphml = s"""<?xml version="1.0" encoding="UTF-8" standalone="no"?>
        <graphml xmlns="http://graphml.graphdrawing.org/xmlns" xmlns:java="http://www.yworks.com/xml/yfiles-common/1.0/java" xmlns:sys="http://www.yworks.com/xml/yfiles-common/markup/primitives/2.0" xmlns:x="http://www.yworks.com/xml/yfiles-common/markup/2.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:y="http://www.yworks.com/xml/graphml" xmlns:yed="http://www.yworks.com/xml/yed/3" xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.1/ygraphml.xsd">
        <key attr.name="Description" attr.type="string" for="graph" id="d0"/>
        <key for="port" id="d1" yfiles.type="portgraphics"/>
        <key for="port" id="d2" yfiles.type="portgeometry"/>
        <key for="port" id="d3" yfiles.type="portuserdata"/>
        <key attr.name="url" attr.type="string" for="node" id="d4"/>
        <key attr.name="description" attr.type="string" for="node" id="d5"/>
        <key for="node" id="d6" yfiles.type="nodegraphics"/>
        <key for="graphml" id="d7" yfiles.type="resources"/>
        <key attr.name="DataMaps" attr.type="string" for="edge" id="d8">
        <default/>
        </key>
        <key attr.name="url" attr.type="string" for="edge" id="d9"/>
        <key attr.name="description" attr.type="string" for="edge" id="d10"/>
        <key for="edge" id="d11" yfiles.type="edgegraphics"/>
        <graph edgedefault="directed" id="G">
        <data key="d0"/>
        """
        for(s <- pd.subjectList){
            var sid = s"n$si"
            graphml +=s"""<node id="${sid}" yfiles.foldertype="group">
            """
            subid2gsid += (s.id.toString -> sid)
            si += 1
            var name = s.name
            if(s.name == "Subject"){
                name = s.name + si
            }
            if(s.inputPool > 0){
                name = name + s"\n{IPSize -> ${s.inputPool}}"
            }
            graphml += s"""<data key="d4"/>
            <data key="d5"><![CDATA[Subject]]></data>
            <data key="d6">
            <y:ProxyAutoBoundsNode>
            <y:Realizers active="0">
            <y:GroupNode>
            <y:Geometry height="797" width="305" x="0" y="0"/>
            <y:Fill color="#F5F5F5" transparent="false"/>
            <y:BorderStyle color="#000000" type="line" width="${isStartSubject(s.startSubject)}"/>"
            <y:NodeLabel alignment="right" autoSizePolicy="node_width" backgroundColor="#EBEBEB" borderDistance="0.0" fontFamily="Dialog" fontSize="15" fontStyle="plain" hasLineColor="false" height="22" horizontalTextPosition="center" iconTextGap="4" modelName="internal" modelPosition="t" textColor="#000000" verticalTextPosition="bottom" visible="true" width="305" x="0" y="0">${name}</y:NodeLabel>
            <y:Shape type="roundrectangle"/>
            <y:State closed="false" closedHeight="50.0" closedWidth="50.0" innerGraphDisplayEnabled="false"/>
            <y:Insets bottom="15" bottomF="15.0" left="15" leftF="15.0" right="15" rightF="15.0" top="15" topF="15.0"/>
            <y:BorderInsets bottom="84" bottomF="83.56040736607167" left="58" leftF="58.39128778752678" right="59" rightF="58.80847062424317" top="1" topF="1.1319652718644306"/>
            </y:GroupNode>
            <y:GroupNode>
            <y:Geometry height="64.0" width="158.0" x="990" y="0.0"/>
            <y:Fill color="#00FFFF" transparent="false"/>
            <y:BorderStyle color="#000000" type="line" width="${isStartSubject(s.startSubject)}"/>
            <y:NodeLabel alignment="center" autoSizePolicy="node_width" backgroundColor="#EBEBEB" borderDistance="0.0" fontFamily="Dialog" fontSize="15" fontStyle="plain" hasLineColor="false" height="22" horizontalTextPosition="center" iconTextGap="4" modelName="internal" modelPosition="c" textColor="#000000" verticalTextPosition="bottom" visible="true" width="158" x="0" y="0">${name}</y:NodeLabel>
            ${multiSubjectString(s.isMultiSubject)}
            <y:Shape type="roundrectangle"/>
            <y:State closed="true" closedHeight="64.0" closedWidth="158.0" innerGraphDisplayEnabled="false"/>
            <y:Insets bottom="5" bottomF="5.0" left="5" leftF="5.0" right="5" rightF="5.0" top="5" topF="5.0"/>
            <y:BorderInsets bottom="54" b0ottomF="54.0" left="0" leftF="0.0" right="148" rightF="148.0" top="0" topF="0.0"/>
            </y:GroupNode>
            </y:Realizers>
            </y:ProxyAutoBoundsNode>
            </data>
            <graph edgedefault="directed" id="${sid}:">
            """
            var gi = 0
            for(g <- s.stateList){
                var gid = s"n$gi"
                graphml += s"""<node id="${sid}::${gid}">
                    """
                stid2gstid += (s.id.toString + "_" + g.data.id.toString -> gid)
                gi += 1
                graphml += s"""<data key="d4"/>
                    """
                if(g.data.stateType.equals("End")){
                    graphml += s"""<data key="d5"><![CDATA[End]]></data>
                    <data key="d6">
                    <y:GenericNode configuration="com.yworks.bpmn.Event.withShadow">
                    <y:Geometry height="30.0" width="30.0" x="0" y="0"/>
                    <y:Fill color="#FFFFFFE6" color2="#D4D4D4CC" transparent="false"/>
                    <y:BorderStyle color="#000000" type="line" width="1.0"/>
                    <y:NodeLabel alignment="center" autoSizePolicy="content" fontFamily="Dialog" fontSize="12" fontStyle="plain" hasBackgroundColor="false" hasLineColor="false" hasText="false" height="4.0" horizontalTextPosition="center" iconTextGap="4" modelName="custom" textColor="#000000" verticalTextPosition="bottom" visible="true" width="4.0" x="0" y="0">
                    <y:LabelModel>
                    <y:SmartNodeLabelModel distance="4.0"/>
                    </y:LabelModel>
                    <y:ModelParameter>
                    <y:SmartNodeLabelModelParameter labelRatioX="0.0" labelRatioY="-0.5" nodeRatioX="0.0" nodeRatioY="0.5" offsetX="0.0" offsetY="4.0" upX="0.0" upY="-1.0"/>
                    </y:ModelParameter>
                    </y:NodeLabel>
                    <y:StyleProperties>
                    <y:Property class="java.awt.Color" name="com.yworks.bpmn.icon.line.color" value="#000000"/>
                    <y:Property class="com.yworks.yfiles.bpmn.view.EventCharEnum" name="com.yworks.bpmn.characteristic" value="EVENT_CHARACTERISTIC_END"/>
                    <y:Property class="java.awt.Color" name="com.yworks.bpmn.icon.fill2" value="#d4d4d4cc"/>
                    <y:Property class="java.awt.Color" name="com.yworks.bpmn.icon.fill" value="#ffffffe6"/>
                    <y:Property class="com.yworks.yfiles.bpmn.view.BPMNTypeEnum" name="com.yworks.bpmn.type" value="EVENT_TYPE_PLAIN"/>
                    </y:StyleProperties>
                    </y:GenericNode>
                    </data>
                    """
                }else{
                    if(g.data.stateType == "Action"){
                        graphml += """<data key="d5"><![CDATA[Internal Action]]></data>"""
                    }else if(g.data.stateType == "ModalSplit" || g.data.stateType == "ModalJoin"){
                        graphml += """<data key="d5"><![CDATA[Predefined Internal Action]]></data>"""
                    }
                    var stateName = ""
                    if(g.data.priority > 0){
                        stateName = s"PRIO:${g.data.priority}\n${g.data.stateName}"
                    }else{
                        stateName = g.data.stateName
                    }
                    graphml += """<data key="d6">
                    <y:GenericNode configuration="com.yworks.bpmn.Activity.withShadow">
                    <y:Geometry height="55.0" width="145.396484375" x="0" y="0"/>
                    <y:Fill color="#FFFFFFE6" color2="#D4D4D4CC" transparent="false"/>
                    """
                    if(g.data.isStartState){
                        graphml += """<y:BorderStyle color="#123EA2" type="line" width="3.0"/>
                    """
                    }else{
                        graphml += """<y:BorderStyle color="#123EA2" type="line" width="1.0"/>
                    """
                    }
                    graphml += s"""<y:NodeLabel alignment="center" autoSizePolicy="content" fontFamily="Dialog" fontSize="12" fontStyle="plain" hasBackgroundColor="false" hasLineColor="false" height="33" horizontalTextPosition="center" iconTextGap="4" modelName="custom" textColor="#000000" verticalTextPosition="bottom" visible="true" width="135" x="0" y="0">
${stateName}
<y:LabelModel>
                    <y:SmartNodeLabelModel distance="4.0"/>
                    </y:LabelModel>
                    <y:ModelParameter>
                    <y:SmartNodeLabelModelParameter labelRatioX="0.0" labelRatioY="0.0" nodeRatioX="0.0" nodeRatioY="0.0" offsetX="0.0" offsetY="0.0" upX="0.0" upY="-1.0"/>
                    </y:ModelParameter>
                    </y:NodeLabel>
                    ${styleProperties(g.data.getStateType)}
                    </y:GenericNode>
                    </data>
                    """
                }
                graphml += "</node>"
            }
            graphml += "</graph>"
            graphml += "</node>"
        }
        for(s <- pd.subjectList){
            var ei = 0
            val transitionsList: ListBuffer[TransitionData] = ListBuffer()
            for(state <- s.stateMap.values){
                state.data.directChildrenTransitionsMap.foreach(f => transitionsList ++= f._2)
                state.data.nonDirectChildrenTransitionsMap.foreach(f => transitionsList ++= f._2)
            }
            for(e <- transitionsList){
                val eid = s"e$ei"
                val sid = subid2gsid(s.id.toString)
                val id = s"$sid::$eid"
                val source = s"$sid::${stid2gstid(s.id.toString + "_" + e.source.toString)}"
                val target = s"$sid::${stid2gstid(s.id.toString + "_" + e.target.toString)}"
                var label = ""
                if(e.priority > 0){
                    label = s"PRIO:${e.priority}\n${e.label}"
                }else{
                    label = e.label
                }

                if(s.stateMap(e.source).data.stateType == "Send" && e.transitionType != "cancel"){
                    var subName = e.information.relatedSubjectName
                    var messageType = e.information.relatedMessageType.replaceAll(" ", "")
                    label = messageType + " to " + subName
                    if(e.label != ""){
                        label = e.label
                    }
                }
                if(s.stateMap(e.source).data.stateType == "Receive" && e.transitionType != "cancel"){
                    var subName = e.information.relatedSubjectName
                    var messageType = e.information.relatedMessageType.replaceAll(" ", "")
                    label = messageType + " from " + subName
                    if(e.label != ""){
                        label = e.label
                    }
                }

                ei += 1
                graphml += s"""
                <edge id="${id}" source="${source}" target="${target}">
                <data key="d9"/>
                <data key="d11">
                <y:GenericEdge configuration="com.yworks.bpmn.Connection">
                <y:Path sx="0.0" sy="0" tx="0.0" ty="0"/>
                <y:LineStyle color="${getLineColor(e.transitionType)}" type="${getLineType(e.transitionType)}" width="1.0"/>
                <y:Arrows source="none" target="delta"/>
                <y:EdgeLabel alignment="left" backgroundColor="#FFFFFF" configuration="AutoFlippingLabel" distance="2.0" fontFamily="Dialog" fontSize="12" fontStyle="plain" height="0" horizontalTextPosition="center" iconTextGap="4" lineColor="#000000" modelName="center_slider" preferredPlacement="anywhere" ratio="0.5" textColor="#000000" verticalTextPosition="bottom" visible="true" width="0" x="0" y="0">${label}<y:PreferredPlacementDescriptor angle="0.0" angleOffsetOnRightSide="0" angleReference="absolute" angleRotationOnRightSide="co" distance="-1.0" frozen="true" placement="anywhere" side="anywhere" sideReference="relative_to_edge_flow"/>
                </y:EdgeLabel>
                <y:StyleProperties>
                <y:Property class="com.yworks.yfiles.bpmn.view.BPMNTypeEnum" name="com.yworks.bpmn.type" value="CONNECTION_TYPE_SEQUENCE_FLOW"/>
                </y:StyleProperties>
                </y:GenericEdge>
                </data>
                </edge>
                """
            }
        }
        val subtosubedgeMap: Map[(String, String), Set[String]] = Map()
        for(s <- pd.subjectList){
            for(e <- s.arrowMap.valuesIterator){
                if(s.stateMap(e.data.source).data.stateType == "Send" && e.data.transitionType == "normal"){
                    var target = 0
                    for(ss <- pd.subjectList){
                        if(e.data.information.relatedSubjectName == ss.name){
                            target = ss.id
                        }
                    }
                    val source = e.subjectID
                    val key = (subid2gsid(source), subid2gsid(target.toString))
                    if(subtosubedgeMap.contains(key)){
                        subtosubedgeMap(key) += s"-${e.data.information.relatedMessageType.replaceAll(" ", "")}"
                    }else{
                        val msgl: Set[String] = Set()
                        msgl += s"-${e.data.information.relatedMessageType.replaceAll(" ", "")}"
                        subtosubedgeMap += (key -> msgl)
                    }
                }
            }
        }
        var ei = 0
        for(k <- subtosubedgeMap.keys){
            println(s"message edge: $k")
            var eid = s"e${ei}"
            ei += 1
            var msg = ""
            for(i <- subtosubedgeMap(k)){
                if(i.equals(subtosubedgeMap(k).head)){

                    msg += s"${i}"
                }else{
                    msg += s"\n${i}"
                }

            }
            graphml += s"""
                <edge id="${eid}" source="${k._1}" target="${k._2}">
                    <data key="d9"/>
                    <data key="d10">
                        <![CDATA[Message edge]]>
                    </data>
                    <data key="d11">
                        <y:GenericEdge configuration="com.yworks.bpmn.Connection">
                            <y:Path sx="0" sy="0" tx="0" ty="0"/>
                            <y:LineStyle color="#000000" type="line" width="1.0"/>
                            <y:Arrows source="transparent_circle" target="delta"/>
                            <y:EdgeLabel alignment="left" backgroundColor="#FFFFFF" configuration="AutoFlippingLabel" distance="2.0" fontFamily="Dialog" fontSize="12" fontStyle="plain" height="33.40234375" horizontalTextPosition="center" iconTextGap="4" lineColor="#000000" modelName="center_slider" preferredPlacement="anywhere" ratio="0.5" textColor="#000000" verticalTextPosition="bottom" visible="true" width="96.73046875" x="137.1062607920527" y="-16.701172858669423">${msg.trim}<y:PreferredPlacementDescriptor angle="0.0" angleOffsetOnRightSide="0" angleReference="absolute" angleRotationOnRightSide="co" distance="-1.0" frozen="true" placement="anywhere" side="anywhere" sideReference="relative_to_edge_flow"/>
                            </y:EdgeLabel>
                            <y:StyleProperties>
                                <y:Property class="com.yworks.yfiles.bpmn.view.BPMNTypeEnum" name="com.yworks.bpmn.type" value="CONNECTION_TYPE_MESSAGE_FLOW"/>
                            </y:StyleProperties>
                        </y:GenericEdge>
                    </data>
                </edge>
                """
        }

        graphml += """
        </graph>
        <data key="d7">
        <y:Resources/>
        </data>
        </graphml>
        """
        graphml

    }
    def isStartSubject(s: Boolean):String = {
        if(s){
            "3.0"
        }else{
            "1.0"
        }
    }

    def multiSubjectString(m: Boolean): String = {
        if(m){
            """
              |<y:NodeLabel alignment="center" autoSizePolicy="content" fontFamily="Dialog" fontSize="16" fontStyle="bold" hasBackgroundColor="false" hasLineColor="false" height="23.6015625" horizontalTextPosition="center" iconTextGap="4" modelName="custom" textColor="#000000" verticalTextPosition="bottom" visible="true" width="17.328125" x="70.3359375" y="4.0">M<y:LabelModel>
              |<y:SmartNodeLabelModel distance="4.0"/>
              |</y:LabelModel>
              |<y:ModelParameter>
              |<y:SmartNodeLabelModelParameter labelRatioX="0.0" labelRatioY="-0.5" nodeRatioX="0.0" nodeRatioY="-0.5" offsetX="0.0" offsetY="4.0" upX="0.0" upY="-1.0"/>
              |</y:ModelParameter>
              |</y:NodeLabel>
            """.stripMargin
        }else{
            ""
        }
    }

    def getLineColor(t: String): String =
        t match {
            case "normal" =>
                "#000000"
            case "timeout" =>
                "#0000ff"
            case "auto" =>
                "#00ff00"
            case "cancel" =>
                "#ff0000"
            case _ =>
                "#000000"
        }

    def getLineType(t: String): String =
        t match {
            case "hidden" =>
                "dashed"
            case _ =>
                "line"
        }


    def styleProperties(t: StateType): String = {
            s"""
               |<y:StyleProperties>
               |            <y:Property class="java.awt.Color" name="com.yworks.bpmn.icon.line.color" value="#000000"/>
               |            ${getStateTypeString(t)}
               |            <y:Property class="java.awt.Color" name="com.yworks.bpmn.icon.fill2" value="#d4d4d4cc"/>
               |            <y:Property class="java.awt.Color" name="com.yworks.bpmn.icon.fill" value="#ffffffe6"/>
               |            <y:Property class="com.yworks.yfiles.bpmn.view.BPMNTypeEnum" name="com.yworks.bpmn.type" value="ACTIVITY_TYPE"/>
               |            <y:Property class="com.yworks.yfiles.bpmn.view.ActivityTypeEnum" name="com.yworks.bpmn.activityType" value="ACTIVITY_TYPE_TASK"/>
               |        </y:StyleProperties>
        """.stripMargin
//        }
    }
    def getStateTypeString(t: StateType): String = {
        if(t.isInstanceOf[FunctionType]){
            """
              |<y:Property class="com.yworks.yfiles.bpmn.view.TaskTypeEnum" name="com.yworks.bpmn.taskType" value="TASK_TYPE_SCRIPT"/>
            """.stripMargin
        }else{
            t match {
                case Action =>
                    """
                      |<y:Property class="com.yworks.yfiles.bpmn.view.TaskTypeEnum" name="com.yworks.bpmn.taskType" value="TASK_TYPE_SERVICE"/>
                    """.stripMargin
                case Send =>
                    """
                      |<y:Property class="com.yworks.yfiles.bpmn.view.TaskTypeEnum" name="com.yworks.bpmn.taskType" value="TASK_TYPE_SEND"/>
                    """.stripMargin
                case Receive =>
                    """
                      |<y:Property class="com.yworks.yfiles.bpmn.view.TaskTypeEnum" name="com.yworks.bpmn.taskType" value="TASK_TYPE_RECEIVE"/>
                    """.stripMargin
                case OpenAllIPs =>
                    """
                      |<y:Property class="com.yworks.yfiles.bpmn.view.TaskTypeEnum" name="com.yworks.bpmn.taskType" value="TASK_TYPE_SCRIPT"/>
                    """.stripMargin
                case CloseIP =>
                    """
                      |<y:Property class="com.yworks.yfiles.bpmn.view.TaskTypeEnum" name="com.yworks.bpmn.taskType" value="TASK_TYPE_SCRIPT"/>
                    """.stripMargin
                case _ => {
                    println(s"type match error: $t")
                    ""
                }

            }
        }

    }
}
