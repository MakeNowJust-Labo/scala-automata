package codes.quine.labo.automata

import scala.collection.MultiDict

final case class NFA[Q, A](delta: MultiDict[(Q, A), Q], I: Set[Q], F: Set[Q]) {
  lazy val Q: Set[Q] = delta.keySet.map(_._1).toSet | delta.values.toSet | I | F
  lazy val Sigma: Set[A] = delta.keySet.map(_._2).toSet

  def mapQ[R](f: Q => R): NFA[R, A] =
    NFA(delta.map { case (q1, a) -> q2 => (f(q1), a) -> f(q2) }, I.map(f), F.map(f))

  def mapSigma[B](f: A => B): NFA[Q, B] =
    NFA(delta.map { case (q1, a) -> q2 => (q1, f(a)) -> q2 }, I, F)
}

object NFA extends NFAInstances0 {
  def reverse[Q, A](nfa: NFA[Q, A]): NFA[Q, A] =
    NFA(nfa.delta.map { case (q1, a) -> q2 => (q2, a) -> q1 }, nfa.F, nfa.I)
}

private[automata] trait NFAInstances0 {
  private[this] val FAInsranceForNFAAny: FiniteAutomata.Aux[NFA[Any, Any], Set[Any], Any] =
    new FiniteAutomata[NFA[Any, Any]] {
      type State = Set[Any]
      type Alphabet = Any
      def stateSet(fa: NFA[Any, Any]): Set[State] = fa.Q.subsets.toSet
      def alphabet(fa: NFA[Any, Any]): Set[Alphabet] = fa.Sigma
      def transition(fa: NFA[Any, Any])(q: State, a: Alphabet): State =
        q.flatMap(q => fa.delta.get((q, a)))
      def initState(fa: NFA[Any, Any]): State = fa.I
      def isAcceptState(fa: NFA[Any, Any])(q: State): Boolean = q.exists(q => fa.F.contains(q))
    }

  implicit def FAInstanceForNFA[Q, A]: FiniteAutomata.Aux[NFA[Q, A], Set[Q], A] =
    FAInsranceForNFAAny.asInstanceOf[FiniteAutomata.Aux[NFA[Q, A], Set[Q], A]]
}
