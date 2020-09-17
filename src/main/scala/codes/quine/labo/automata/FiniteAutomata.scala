package codes.quine.labo.automata

trait FiniteAutomata[FA] {
  type State
  type Alphabet
  def stateSet(fa: FA): Set[State]
  def alphabet(fa: FA): Set[Alphabet]
  def transition(fa: FA)(q: State, a: Alphabet): State
  def initState(fa: FA): State
  def isAcceptState(fa: FA)(q: State): Boolean
}

object FiniteAutomata {
  type Aux[FA, Q, A] = FiniteAutomata[FA] {
    type State = Q
    type Alphabet = A
  }

  def apply[FA, Q, A](implicit FA: Aux[FA, Q, A]): Aux[FA, Q, A] = FA

  def execute[FA, Q, A](fa: FA)(as: List[A])(implicit FA: Aux[FA, Q, A]): Boolean =
    FA.isAcceptState(fa)(as.foldLeft(FA.initState(fa))(FA.transition(fa)(_, _)))
}
