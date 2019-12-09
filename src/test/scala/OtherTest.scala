import io.circe._
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import de.tkip.sbpm.frontend.Data._
import io.circe.generic.JsonCodec
import io.circe._, io.circe.generic.semiauto._
import io.circe.parser._

import scala.collection.mutable.ListBuffer

object OtherTest extends App {

    @JsonCodec sealed trait SaveData

    case class ProcessSavaData(id: Int, name: String, m: Map[Int, StateSaveDate]) extends SaveData

    case class TransitionSaveData(id: Int, name: String) extends SaveData

    case class StateSaveDate(id: Int, name: String, isStartState: Boolean, directChildrenTransitionsList: ListBuffer[TransitionSaveData]) extends SaveData

    implicit val processSaveDataDecoder: Decoder[ProcessSavaData] = deriveDecoder[ProcessSavaData]
    implicit val StateSaveDataDecoder: Decoder[StateSaveDate] = deriveDecoder[StateSaveDate]
    implicit val TransitionSaveDataDecoder: Decoder[TransitionSaveData] = deriveDecoder[TransitionSaveData]

    implicit val processSaveDataEncoder: Encoder[ProcessSavaData] = deriveEncoder[ProcessSavaData]


    val t = TransitionSaveData(11, "1111")
    val lt = ListBuffer[TransitionSaveData]()
    lt += t
    lt += t
    val sd = StateSaveDate(222, "222", false, lt)
    var testMap: Map[Int, StateSaveDate] = Map()
    testMap += (1 -> sd)
    val pd = ProcessSavaData(333, "333", testMap)

    //  val s = StateSaveDate(22, "2222", false, lt)

    val json = pd.asJson.noSpaces
    //  println(json)

    val j = parse(json)
    println(j)
    println("-----------------------------")
    println(j.right.get)
    val decodedFoo = processSaveDataDecoder.decodeJson(j.right.get)
    println(decodedFoo)

    //  val graphList: ListBuffer[StateGraph] = ListBuffer()
    //  graphList += new Action(new StateData(1))
    //  graphList += new Receive(new StateData(2))
    //  graphList += new Send(new StateData(3))
    //  graphList += new End(new StateData(4))
    //
    //  val path = "E:\test.txt"
    //  val out = new ObjectOutputStream(new FileOutputStream(path))
    //  out.writeObject(graphList)
    //  out.close()
    //
    //  val in = new ObjectInputStream(new FileInputStream(path))
    //  val readList = in.readObject()
    //  in.close()
    //
    //  assert(graphList == readList)
}
