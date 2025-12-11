package sutd.compiler.simp.lattice

import sutd.compiler.simp.lattice.CompleteLattice.{given, *} 

object SignLattice {
    import CompleteLattice.* 
    enum SignAbsVal {
        case Bot    // _|_
        case Minus  // -
        case Plus   // + 
        case Top    // T
        case Zero   // 0
    }

    import SignAbsVal.*
    // Cohort Problem Exercise 2
    given signLattice:CompleteLattice[SignAbsVal] = new CompleteLattice[SignAbsVal] {
        def sqSubSetEq(a: SignAbsVal, b: SignAbsVal): Option[Boolean] = (a, b) match {
            case (Bot, _) => Some(true)
            case (_, Top) => Some(true)
            case (x, y) if x == y => Some(true)
            case (Top, _) => Some(false)
            case (_, Bot) => Some(false)
            case _ => None
        }
        def lub(a:SignAbsVal, b:SignAbsVal):SignAbsVal = (a, b) match {
            case (Bot, x) => x
            case (x, Bot) => x
            case (Top, _) => Top
            case (_, Top) => Top
            case (x, y) if x == y => x
            case _ => Top
        }
    }
}