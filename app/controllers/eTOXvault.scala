package controllers

import play.api.libs.json._
import play.api.libs.ws._

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
import models.dataframe.DataFrame
import models.dataframe._

object eTOXvault {

//  private val model_units_init = {
//    val mp = Map("/Toxicity/Organ Toxicity/Cardiotoxicity/QT Prolongation/7" -> "LQTinc units",
//      "/Toxicity/Organ Toxicity/Cardiotoxicity/hERG inhibition/3" -> "hERG inhibition units",
//      "/Toxicity/Organ Toxicity/Cardiotoxicity/Cav1.2 inhibition/1" -> "Cav1.2 inhibition units",
//      "/Toxicity/Organ Toxicity/Cardiotoxicity/QT Prolongation/8" -> "LQTpot units",
//      "/Toxicity/Organ Toxicity/Cardiotoxicity/KCNQ1 inhibition/1" -> "KCNQ1 inhibition units")
//
//    val l = for ((tag, units) <- mp) yield (Map("tag" -> tag, "units" -> units))
//    val l2 = l.toList
//    DataFrame(l2)
//  }
  val model_units = DataFrame(Application.envoy_ws_home + "/data/model_units.txt")

  def getModelInfo(tag: String) = {

    val tagencoded = tag.replaceAll(" ", "%20")

    val model_json_url = "http://lamia.upf.edu/modelinfo/?modeltag=" + tagencoded + "&authkey=7b80f381248245c4&provider=FIMIM"
    println("Model info etox vault: ")
    println(model_json_url)
    implicit val context = scala.concurrent.ExecutionContext.Implicits.global

    val futureResult_url = WS
      .url(model_json_url)

    val jso = try {
      val futureResult = futureResult_url.get()
      val response = Await.result(futureResult, 10 seconds)

      val jso = Json.parse(response.body).as[JsObject]
      //val mp = for ((field, value) <- jso.value if (field != "software"))        yield ((field, value.as[JsString].value))
      jso
    } catch {
      case e: Exception => play.api.libs.json.Json.toJson(Map("error" -> "etoxault erroneus response")).as[JsObject]
    }

    jso
  }

  def getModelInfoMP(tag: String) = {

    println("Model tag: " + tag)
    val jso = this.getModelInfo(tag)
    val mp = for ((field, value) <- jso.value if (field != "software"))
      yield ((field, value.as[JsString].value))

    val df = DataFrame(List(collection.immutable.HashMap(mp.toSeq: _*)))
    //println("Fields:")
    //df.getFields(List()).map(println)
    df.join_left(model_units, "modeltag", "tag").getData(0)
  }
}
