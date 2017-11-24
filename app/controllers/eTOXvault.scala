package controllers

import play.api.libs.json._
import play.api.libs.ws._

import scala.concurrent.Await
import scala.concurrent.duration._
import models.dataframe.DataFrame
import models.dataframe._

object eTOXvault {

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

  def loadModels = {
    import java.io._
    controllers.Application.conf
    val models = eTOXlab.read_models
    val infomodels = for ((short, tag, _, _) <- models)
      yield ((short, getModelInfo(tag)))

    val modelinfodir = controllers.Application.envoy_ws_home + "/modelinfo/"
    for ((short, info) <- infomodels) {
      val pw = new PrintWriter(new File(modelinfodir + short + ".txt"))
      pw.write(info.toString())
      pw.close
    }
    models
  }
  def getModelInfoLocal(tag: String) = {
    val models = eTOXlab.read_models    
    val (short, _, _, _) = models.filter(_._2 == tag).head
    val modelinfodir = controllers.Application.envoy_ws_home + "/modelinfo/"
    val s=scala.io.Source.fromFile(modelinfodir + short + ".txt").mkString
    s
  }

  def getModelInfoMP(tag: String) = {
    val tagencoded = tag.replaceAll("%20"," ")
    println("Model tag: " + tagencoded)
    val info=this.getModelInfoLocal(tagencoded)
    println("Info: "+ info)
    val jso =  Json.parse(info).as[JsObject]
    val mp = for ((field, value) <- jso.value if (field != "software"))
      yield (field, value.as[JsString].value)

    //val jso = this.getModelInfo(tag)
    //val mp = for ((field, value) <- jso.value if (field != "software"))
    //      yield (field, value.as[JsString].value)
      
    val df = DataFrame(List(collection.immutable.HashMap(mp.toSeq: _*)))
    df.getData(0)
  }
}
