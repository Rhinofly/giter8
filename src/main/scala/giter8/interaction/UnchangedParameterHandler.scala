package giter8.interaction

trait UnchangedParameterHandler {
  
  def handle(params:Map[String, String]):Map[String, String]
}