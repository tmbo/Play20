package play.api.libs.iteratee

object Traversable {

  def passAlong[M] = new Enumeratee[M, M] {
    def apply[A](it: Iteratee[M, A]): Iteratee[M, Iteratee[M, A]] = {
      it.mapDone(a => Done(a, Input.Empty))

    }

  }

  trait CheckDone[From, To] extends Enumeratee[From, To] {

    def continue[A](k: Input[To] => Iteratee[To, A]): Iteratee[From, Iteratee[To, A]]

    def apply[A](it: Iteratee[To, A]): Iteratee[From, Iteratee[To, A]] = {

      def step(in: Input[From]): Iteratee[From, Iteratee[To, A]] = {
        in match {

          case Input.El(e) => it.pureFlatFold(
            (_, _) => Done(it, in),
            k => continue(k),
            (_, _) => Done(it, in))

          case Input.EOF => Done(it, Input.EOF)

          case Input.Empty => Cont(step)
        }

      }
      Cont(step)
    }

  }

  def takeUpTo[M](count: Int)(implicit p: M => scala.collection.TraversableLike[_, M]): Enumeratee[M, M] = new Enumeratee[M, M] {

    def apply[A](it: Iteratee[M, A]): Iteratee[M, Iteratee[M, A]] = {

      def step(inner: Iteratee[M, A], leftToTake: Int)(in: Input[M]): Iteratee[M, Iteratee[M, A]] = {
        in match {
          case in @ Input.El(e) =>
            inner.pureFlatFold(
              (_, _) => Done(inner, in),
              k => e.splitAt(leftToTake) match {
                case (all, x) if x.isEmpty => Cont(step(k(Input.El(all)), (leftToTake - all.size)))
                case (x, left) if x.isEmpty => Done(inner, Input.El(left))
                case (toPush, left) => Done(k(Input.El(toPush)), Input.El(left))
              },
              (_, _) => Done(inner, in))

          case Input.EOF => Done(inner, Input.EOF)

          case Input.Empty => Cont(step(inner, leftToTake))
        }

      }
      Cont(step(it, count))

    }
  }

  def take[M](count: Int)(implicit p: M => scala.collection.TraversableLike[_, M]): Enumeratee[M, M] = new Enumeratee[M, M] {

    def apply[A](it: Iteratee[M, A]): Iteratee[M, Iteratee[M, A]] = {

      def step(inner: Iteratee[M, A], leftToTake: Int)(in: Input[M]): Iteratee[M, Iteratee[M, A]] = {
        in match {
          case in @ Input.El(e) =>
            e.splitAt(leftToTake) match {
              case (all, x) if x.isEmpty => inner.pureFlatFold(
                (_, _) => Cont(step(inner, (leftToTake - all.size))),
                k => Cont(step(k(Input.El(all)), (leftToTake - all.size))),
                (_, _) => Cont(step(inner, (leftToTake - all.size))))
              case (x, left) if x.isEmpty => Done(inner, Input.El(left))
              case (toPush, left) => Done(inner.pureFlatFold((_, _) => inner, k => k(Input.El(toPush)), (_, _) => inner), Input.El(left))
            }

          case Input.EOF => Done(inner, Input.EOF)

          case Input.Empty => Cont(step(inner, leftToTake))
        }

      }
      Cont(step(it, count))

    }
  }

  def drop[M](count: Int)(implicit p: M => scala.collection.TraversableLike[_, M]): Enumeratee[M, M] = new Enumeratee[M, M] {

    def apply[A](inner: Iteratee[M, A]): Iteratee[M, Iteratee[M, A]] = {

      def step(it: Iteratee[M, A], leftToDrop: Int)(in: Input[M]): Iteratee[M, Iteratee[M, A]] = {
        in match {
          case in @ Input.El(e) =>
            val left = leftToDrop - e.size
            left match {
              case i if i > 0 => Cont(step(it, left))
              case i =>
                val toPass = if (i < 0) Input.El(e.drop(leftToDrop)) else Input.Empty
                it.pureFlatFold(
                  (_, _) => Done(it, toPass),
                  k => passAlong(k(toPass)),
                  (_, _) => Done(it, toPass))

            }
          case Input.Empty => Cont(step(it, leftToDrop))

          case Input.EOF => Done(it, Input.EOF)
        }
      }

      Cont(step(inner, count))

    }
  }
}
