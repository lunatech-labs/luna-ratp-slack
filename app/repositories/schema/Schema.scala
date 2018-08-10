package repositories

package object Schema{
  type Schema = Option[String]
  implicit val name: Schema = Some("ratpbot")
}
