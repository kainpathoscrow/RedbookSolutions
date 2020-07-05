// Assuming the following representation of Prop, implement && as a method of Prop.

// Solution
trait Prop { 
  def check: Boolean 
  def &&(p: Prop): Prop = {
    def thisCheck = check
    new Prop {
      def check = thisCheck && p.check
    }
  }
}

// Tests
val propT = new Prop { def check = {println(true); true} }
val propF = new Prop { def check = {println(false); false} }

assert(propT.check)
assert(!propF.check)
assert(!((propT && propF).check))
assert(!((propF && propT).check))

println("Prints nothing until check is called:")
val propCombined = propT && propF
println("Now prints true \\n false:")
propCombined.check