package ilc
package feature
package naturals

trait Types extends base.Types {
  case object NatType extends Type { override def toString = "ℕ" }
  val ℕ = NatType
}
