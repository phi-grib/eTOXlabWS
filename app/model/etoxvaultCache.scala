package model

object etoxvaultCache {

  val fileNameCache = controllers.Application.envoy_ws_home + "/etoxvault_cache.txt"
  def load_cache_from_ws= {

    val data = for ((k, (modelid, tag, _, _)) <- controllers.eTOXlab.models)
      yield (
      Map("tag" -> tag,
        "info" -> controllers.eTOXvault.getModelInfo(tag.replaceAll(" ","%20")).toString))
        
    val df=models.dataframe.DataFrame(data.toList)
    df.toText(fileNameCache)
  }
  
}