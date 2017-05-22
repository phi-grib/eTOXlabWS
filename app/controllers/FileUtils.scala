package controllers

import java.util.GregorianCalendar
import java.text.SimpleDateFormat
import java.util.Properties
import java.io.FileInputStream
import scala.collection.mutable.HashMap
import scala.collection.immutable.HashSet

object FileUtils {
  def getFile(fileName: String) = {
    io.Source.fromFile(fileName).getLines().toSet
  }

  def readPropertiesFile(file: String) = {
    var defaultProps = new Properties()
    var in = new FileInputStream(file)
    defaultProps.load(in)
    in.close()
    defaultProps
  }

  def getNewFilename(prefix: String, suffix: String, path: String) = {
    val calendar = new GregorianCalendar()
    val sdf = new SimpleDateFormat("yyyyMMdd_HHmmss_SSS")
    val datetime = sdf.format(calendar.getTime())
    path + "/"+prefix + "_" + datetime.toString() + suffix
  }
}