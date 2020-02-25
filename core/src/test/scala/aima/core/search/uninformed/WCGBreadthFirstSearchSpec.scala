package aima.core.search.uninformed

import aima.core.search.StateNode
import aima.core.search.problems.WolfCabbageGoat._
import org.specs2.mutable.Specification

import scala.reflect.ClassTag
import org.specs2.specification.Scope

class WCGBreadthFirstSearchSpec extends Specification {

  "going from final to the same must return no actions" in new context {
    search(new WolfCabbageGoatProblem(At(Right, Right, Right, Right), At(Right, Right, Right, Right)), NoAction) must beEmpty
  }

  "going Left to Right banks must return a list of actions" in new context {
    search(new WolfCabbageGoatProblem(At(Right, Right, Left, Left), At(Right, Right, Right, Right)), NoAction) must_== List(MoveGoat)
  }

  "going Left to Right banks must return a list of actions" in new context {
    search(new WolfCabbageGoatProblem(At(Left, Left, Left, Left), At(Right, Right, Right, Right)), NoAction) must_==
      List(MoveGoat, MoveBoat, MoveWolf, MoveGoat, MoveCabbage, MoveBoat, MoveGoat)
  }

  trait context extends Scope with BreadthFirstSearch[WolfCabbageGoatState, WolfCabbageGoatAction] {

    override implicit val sCT: ClassTag[WolfCabbageGoatState] = sCTag
    override implicit val aCT: ClassTag[WolfCabbageGoatAction] = aCTag
    override implicit val nCT: ClassTag[StateNode[WolfCabbageGoatState, WolfCabbageGoatAction]] = snCTag
  }

}
