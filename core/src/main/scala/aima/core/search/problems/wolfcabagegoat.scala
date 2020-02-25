package aima.core.search.problems

import aima.core.search.{CostNode, Problem, StateNode}

import scala.reflect.{ClassTag, classTag}

object WolfCabbageGoat {

  sealed trait RiverBank
  case object Left extends RiverBank
  case object Right extends RiverBank


  sealed trait WolfCabbageGoatState
  final case class At(wolf: RiverBank, cabbage: RiverBank, goat: RiverBank, boat: RiverBank) extends WolfCabbageGoatState

  sealed trait WolfCabbageGoatAction
  final case object MoveWolf extends WolfCabbageGoatAction
  final case object MoveCabbage extends WolfCabbageGoatAction
  final case object MoveGoat extends WolfCabbageGoatAction
  final case object MoveBoat extends WolfCabbageGoatAction
  case object NoAction extends WolfCabbageGoatAction

  val sCTag: ClassTag[WolfCabbageGoatState]  = classTag[WolfCabbageGoatState]
  val aCTag: ClassTag[WolfCabbageGoatAction] = classTag[WolfCabbageGoatAction]

  val snCTag: ClassTag[StateNode[WolfCabbageGoatState, WolfCabbageGoatAction]] =
    classTag[StateNode[WolfCabbageGoatState, WolfCabbageGoatAction]]

  val cnCTag: ClassTag[CostNode[WolfCabbageGoatState, WolfCabbageGoatAction]] = classTag[CostNode[WolfCabbageGoatState, WolfCabbageGoatAction]]

  val actionList: List[WolfCabbageGoatAction] = MoveWolf :: MoveCabbage :: MoveGoat :: MoveBoat :: Nil

  class WolfCabbageGoatProblem(val initialState: WolfCabbageGoatState, val goalState: WolfCabbageGoatState)
    extends Problem[WolfCabbageGoatState, WolfCabbageGoatAction] {
    def result(currentState: WolfCabbageGoatState, action: WolfCabbageGoatAction): WolfCabbageGoatState = (currentState, action) match {
      case (At(wolf, cabbage, goat, boat), MoveWolf) => At(opposite(wolf), cabbage, goat, opposite(boat))
      case (At(wolf, cabbage, goat, boat), MoveCabbage) => At(wolf, opposite(cabbage), goat, opposite(boat))
      case (At(wolf, cabbage, goat, boat), MoveGoat)  => At(wolf, cabbage, opposite(goat), opposite(boat))
      case (At(wolf, cabbage, goat, boat), MoveBoat) => At(wolf, cabbage, goat, opposite(boat))
      case (_, NoAction)   => currentState
    }

    def actions(state: WolfCabbageGoatState): List[WolfCabbageGoatAction] = actionList.filter(a => canApply(state, a))

    def isGoalState(state: WolfCabbageGoatState): Boolean = (state, goalState) match {
      case (At(wolf, cabbage, goat, boat), At(wolfG, cabbageG, goatG, boatG)) =>
        wolf == wolfG && cabbage == cabbageG && goat == goatG && boat == boatG
      case _                    => false
    }

    def canApply(state: WolfCabbageGoatState, action: WolfCabbageGoatAction): Boolean = (state, action) match {
      case (At(wolf, cabbage, goat, boat), MoveWolf) if wolf == boat =>
        notDangerous(At(opposite(wolf), cabbage, goat, opposite(boat)))
      case (At(wolf, cabbage, goat, boat), MoveCabbage) if cabbage == boat =>
        notDangerous(At(wolf, opposite(cabbage), goat, opposite(boat)))
      case (At(wolf, cabbage, goat, boat), MoveGoat) if goat == boat =>
        notDangerous(At(wolf, cabbage, opposite(goat), opposite(boat)))
      case (At(wolf, cabbage, goat, boat), MoveBoat) =>
        notDangerous(At(wolf, cabbage, goat, opposite(boat)))
      case _ => false
    }

    def opposite(riverBank: RiverBank): RiverBank = riverBank match {
      case Left => Right
      case Right => Left
    }

    def notDangerous(state: WolfCabbageGoatState): Boolean =  !(wolfEatGoat(state) || goatEatCabbage(state))

    def wolfEatGoat(state: WolfCabbageGoatState): Boolean = state match {
      case At(wolf, _ , goat, boat) => wolf == goat && wolf != boat
    }

    def goatEatCabbage (state: WolfCabbageGoatState): Boolean = state match {
      case At(_, cabbage , goat, boat) => cabbage == goat && cabbage != boat
    }

    //todo
    def stepCost(state: WolfCabbageGoatState, action: WolfCabbageGoatAction, statePrime: WolfCabbageGoatState): Int =
      (state, statePrime) match {
        case _ => 1
      }
  }

}
