package controllers

import play.api.libs.json._
import play.api.libs.ws._

import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._

object eTOXvault {

  def getModelInfo(tag: String) = {

    val model_json_url = "http://lamia.upf.edu/modelinfo/?modeltag=" + tag + "&authkey=7b80f381248245c4&provider=FIMIM"
    implicit val context = scala.concurrent.ExecutionContext.Implicits.global

    val futureResult_url = WS
      .url(model_json_url)

    val futureResult = futureResult_url.get()
    val response = Await.result(futureResult, 10 seconds)

    val jso = Json.parse(response.body).as[JsObject]
    val mp = for ((field, value) <- jso.value if (field != "software"))
      yield ((field, value.as[JsString].value))

    jso
  }

  def getModelInfoMP(tag: String) = {
    val jso = this.getModelInfo(tag)
    val mp = for ((field, value) <- jso.value if (field != "software"))
      yield ((field, value.as[JsString].value))
    mp
  }
}
