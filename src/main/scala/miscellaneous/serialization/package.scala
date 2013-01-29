package miscellaneous

package object serialization {
  import scala.annotation.target._

  type specializedField = specializedFor @field
  type AField = SField @field
}
