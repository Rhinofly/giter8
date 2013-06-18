package giter8.interaction

import giter8.interaction.ExistingFileActionProvider
import giter8.interaction.UnchangedParameterHandler

case class Interaction(
    unchangedParameterHandler:Option[UnchangedParameterHandler] = None,
    existingFileActionProvider:Option[ExistingFileActionProvider] = None
)

object Interaction {
  
  def apply(u:UnchangedParameterHandler, e:ExistingFileActionProvider):Interaction =
    Interaction(Some(u), Some(e))
}