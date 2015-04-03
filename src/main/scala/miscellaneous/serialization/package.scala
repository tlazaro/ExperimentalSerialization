package miscellaneous

import scala.annotation.meta.field

package object serialization {
  type specializedField = specializedFor @field
  type AField = SField @field
}
