package giter8.git

case class Repository(repositories: Seq[String], branch: Option[String])

object Repository {

  val Repo = """^([^\s/]+)/([^\s/]+?)(?:\.g8)?$""".r
  val Branch = """^-(b|-branch)$""".r
  val Git = "^(git[@|://].*)$".r
  val Local = """^file://(\S+)$""".r

  private def apply(repo: String): Repository =
    Repository(Seq(repo), None)

  private def apply(repo: String, branch: Option[String]): Repository =
    Repository(Seq(repo), branch)

  private def apply(user: String, project: String): Repository = 
    Repository(user, project, None)

  private def apply(user: String, project: String, branch: Option[String]): Repository = {
    val open = s"git://github.com/$user/$project.g8.git"
    val ssh = s"git@github.com:$user/$project.g8.git"
    Repository(Seq(open, ssh), branch)
  }

  def get(repositoryPattern: Array[String]): Option[Repository] =
    repositoryPattern match {
      case Array(Local(repo)) =>
        Some(Repository(repo))
      case Array(Local(repo), Branch(_), branch) =>
        Some(Repository(repo, Some(branch)))
      case Array(Repo(user, proj)) =>
        Some(Repository(user, proj))
      case Array(Repo(user, proj), Branch(_), branch) =>
        Some(Repository(user, proj, Some(branch)))
      case Array(Git(remote)) =>
        Some(Repository(remote))
      case Array(Git(remote), Branch(_), branch) =>
        Some(Repository(remote, Some(branch)))
      case _ => 
        None
    }

}