package ilc
package feature
package integers

trait Types extends base.Types {
  case object IntType extends Type {
    override def toString = "Z"
  }

  val ℤ = IntType
}
