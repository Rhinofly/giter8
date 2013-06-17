package giter8

object Regexes {
  val Param = """^--(\S+)=(.+)$""".r
  val Repo = """^([^\s/]+)/([^\s/]+?)(?:\.g8)?$""".r
  val Branch = """^-(b|-branch)$""".r
  val RemoteTemplates = """^-(l|-list)$""".r
  val Git = "^(git[@|://].*)$".r
  val Local = """^file://(\S+)$""".r
}