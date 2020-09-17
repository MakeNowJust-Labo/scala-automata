package codes.quine.labo.automata

import scala.collection.MultiDict
import scala.collection.mutable

final case class DFA[Q, A](delta: Map[(Q, A), Q], i: Q, F: Set[Q]) {
  lazy val Q: Set[Q] = delta.keySet.map(_._1).toSet | delta.values.toSet | Set(i) | F
  lazy val Sigma: Set[A] = delta.keySet.map(_._2).toSet

  def mapQ[R](f: Q => R): DFA[R, A] =
    DFA(delta.map { case (q1, a) -> q2 => (f(q1), a) -> f(q2) }, f(i), F.map(f))

  def mapSigma[B](f: A => B): DFA[Q, B] =
    DFA(delta.map { case (q1, a) -> q2 => (q1, f(a)) -> q2 }, i, F)
}

object DFA extends DFAInstances0 {
  def from[FA](fa: FA)(implicit FA: FiniteAutomaton[FA]): DFA[FA.State, FA.Alphabet] = {
    type Q = FA.State
    type A = FA.Alphabet

    val alphabet = FA.alphabet(fa)

    val delta = Map.newBuilder[(Q, A), Q]
    val i = FA.initState(fa)
    val F = Set.newBuilder[Q]

    val queue = mutable.Queue(i)
    val queued = mutable.Set(i)

    while (queue.nonEmpty) {
      val q1 = queue.dequeue()
      if (FA.isAcceptState(fa)(q1)) {
        F.addOne(q1)
      }
      for (a <- alphabet) {
        val q2 = FA.transition(fa)(q1, a)
        delta.addOne((q1, a) -> q2)
        if (!queued.contains(q2)) {
          queue.enqueue(q2)
          queued.add(q2)
        }
      }
    }

    DFA(delta.result(), i, F.result())
  }

  def reverse[Q, A](dfa: DFA[Q, A]): NFA[Q, A] =
    NFA(
      MultiDict.from(dfa.delta.iterator.map { case (q1, a) -> q2 => (q2, a) -> q1 }),
      dfa.F,
      Set(dfa.i)
    )

  def rename[Q, A](dfa: DFA[Q, A]): DFA[Int, A] = {
    val names = mutable.Map.empty[Q, Int]
    dfa.mapQ(names.getOrElseUpdate(_, names.size))
  }
}

private[automata] trait DFAInstances0 {
  private[this] val FAInstanceForDFAAny: FiniteAutomaton[DFA[Any, Any]] =
    new FiniteAutomaton[DFA[Any, Any]] {
      type State = Option[Any]
      type Alphabet = Any
      def stateSet(fa: DFA[Any, Any]): Set[State] = fa.Q.map(Option(_)) | Set(None)
      def alphabet(fa: DFA[Any, Any]): Set[Alphabet] = fa.Sigma
      def transition(fa: DFA[Any, Any])(q: State, a: Alphabet): State =
        q.flatMap(q => fa.delta.get((q, a)))
      def initState(fa: DFA[Any, Any]): State = Option(fa.i)
      def isAcceptState(fa: DFA[Any, Any])(q: State): Boolean =
        q.exists(fa.F.contains(_))
    }

  implicit def FAInstanceForDFA[Q, A]: FiniteAutomaton[DFA[Q, A]] =
    FAInstanceForDFAAny.asInstanceOf[FiniteAutomaton[DFA[Q, A]]]
}
