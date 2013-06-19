package giter8.interaction

case class Interaction(
    unchangedParameterHandler:Option[UnchangedParameterHandler] = None,
    existingFileActionProvider:Option[ExistingFileActionProvider] = None
)

object Interaction {
  
  def apply(u:UnchangedParameterHandler, e:ExistingFileActionProvider):Interaction =
    Interaction(Some(u), Some(e))
}