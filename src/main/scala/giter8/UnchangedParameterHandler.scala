package giter8

trait UnchangedParameterHandler {
  
  def handle(params:Map[String, String]):Map[String, String]
}