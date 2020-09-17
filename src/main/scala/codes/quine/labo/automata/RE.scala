package codes.quine.labo.automata

import scala.collection.mutable

sealed abstract class RE[+A]

object RE extends REInstances0 {
  final case object Fail extends RE[Nothing]
  final case object Empty extends RE[Nothing]
  final case class Atom[A](a: A) extends RE[A]
  final case class Cat[A](re1: RE[A], re2: RE[A]) extends RE[A]
  final case class Alt[A](re1: RE[A], re2: RE[A]) extends RE[A]
  final case class Star[A](re: RE[A]) extends RE[A]

  def normalize[A](re: RE[A]): RE[A] =
    re match {
      case Cat(re1, re2) =>
        (normalize(re1), normalize(re2)) match {
          case (Fail, _)            => Fail
          case (_, Fail)            => Fail
          case (re1, Empty)         => re1
          case (Empty, re2)         => re2
          case (Cat(re1, re2), re3) => Cat(re1, normalize(Cat(re2, re3)))
          case (re1, re2)           => Cat(re1, re2)
        }
      case Alt(re1, re2) =>
        (normalize(re1), normalize(re2)) match {
          case (re1, Fail)              => re1
          case (Fail, re2)              => re2
          case (re1, re2) if re1 == re2 => re1
          case (re1, re2)               => Alt(re1, re2)
        }
      case Star(re) =>
        normalize(re) match {
          case Fail     => Empty
          case Empty    => Empty
          case Star(re) => Star(re)
          case re       => Star(re)
        }
      case re => re
    }

  def normalizeWithOrdering[A: Ordering](re: RE[A]): RE[A] =
    normalize(re) match {
      case Alt(re1, re2) if Ordering[RE[A]].gt(re1, re2) => Alt(re2, re1)
      case re                                            => re
    }

  def alphabet[A](re: RE[A]): Set[A] =
    re match {
      case Empty         => Set.empty
      case Fail          => Set.empty
      case Atom(a)       => Set(a)
      case Cat(re1, re2) => alphabet(re1) | alphabet(re2)
      case Alt(re1, re2) => alphabet(re1) | alphabet(re2)
      case Star(re1)     => alphabet(re1)
    }

  def isEmpty[A](re: RE[A]): Boolean =
    re match {
      case Fail          => false
      case Empty         => true
      case Atom(a)       => false
      case Cat(re1, re2) => isEmpty(re1) && isEmpty(re2)
      case Alt(re1, re2) => isEmpty(re1) || isEmpty(re2)
      case Star(re)      => true
    }

  def derivate[A](re: RE[A], a: A): RE[A] =
    re match {
      case Fail                          => Fail
      case Empty                         => Fail
      case Atom(a1) if a1 == a           => Empty
      case Atom(_)                       => Fail
      case Alt(re1, re2)                 => Alt(derivate(re1, a), derivate(re2, a))
      case Cat(re1, re2) if isEmpty(re2) => Alt(Cat(derivate(re1, a), re2), derivate(re2, a))
      case Cat(re1, re2)                 => Cat(derivate(re1, a), re2)
      case Star(re)                      => Cat(derivate(re, a), Star(re))
    }
}

private[automata] trait REInstances0 extends REInstances1 {
  import RE._

  implicit def OrderingInstanceForRE[A: Ordering]: Ordering[RE[A]] =
    new Ordering[RE[A]] { self =>
      def compare(re1: RE[A], re2: RE[A]): Int = {
        def rank(re: RE[A]): Int =
          re match {
            case Fail       => 0
            case Empty      => 1
            case _: Atom[A] => 10
            case _: Cat[A]  => 100
            case _: Alt[A]  => 101
            case _: Star[A] => 1000
          }

        (re1, re2) match {
          case (Atom(a1), Atom(a2)) => Ordering[A].compare(a1, a2)
          case (Cat(re11, re12), Cat(re21, re22)) =>
            Ordering.Tuple2(self, self).compare((re11, re12), (re21, re22))
          case (Alt(re11, re12), Alt(re21, re22)) =>
            Ordering.Tuple2(self, self).compare((re11, re12), (re21, re22))
          case (Star(re1), Star(re2)) => compare(re1, re2)
          case (re1, re2)             => rank(re1).compare(rank(re2))
        }
      }
    }

  implicit def FAInstanceForREWithOrdering[A: Ordering]: FiniteAutomata.Aux[RE[A], RE[A], A] =
    new FAInstanceForREImpl(RE.normalizeWithOrdering(_))
}

private[automata] trait REInstances1 {
  implicit def FAInstanceForRE[A]: FiniteAutomata.Aux[RE[A], RE[A], A] =
    new FAInstanceForREImpl(RE.normalize(_))
}

private[automata] final class FAInstanceForREImpl[A](val normalize: RE[A] => RE[A])
    extends FiniteAutomata[RE[A]] {
  type State = RE[A]
  type Alphabet = A
  def alphabet(re: RE[A]): Set[A] = RE.alphabet(re)
  def stateSet(re: RE[A]): Set[RE[A]] = {
    val alphabet = RE.alphabet(re)
    val re0 = normalize(re)
    val set = mutable.Set(re0)
    val queue = mutable.Queue(re0)
    while (queue.nonEmpty) {
      val re1 = queue.dequeue()
      for (a <- alphabet) {
        val re2 = normalize(RE.derivate(re1, a))
        if (set.contains(re2)) {
          set.add(re2)
          queue.enqueue(re2)
        }
      }
    }
    set.toSet
  }
  def initState(re: RE[A]): RE[A] = normalize(re)
  def transition(re0: RE[A])(re: RE[A], a: A): RE[A] = RE.normalize(RE.derivate(re, a))
  def isAcceptState(re0: RE[A])(re: RE[A]): Boolean = RE.isEmpty(re)
}
