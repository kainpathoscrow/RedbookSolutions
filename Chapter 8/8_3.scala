// Assuming the following representation of Prop, implement && as a method of Prop.

// Solution
trait Prop { 
  def check: Boolean 
  def &&(p: Prop): Prop = {
    val thisCheck = check
    new Prop {
      def check = thisCheck && p.check
    }
  }
}

// Tests
val propT = new Prop { def check = true }
val propF = new Prop { def check = false }

assert(propT.check)
assert(!propF.check)
assert(!((propT && propF).check))
assert(!((propF && propT).check))
