package FiveFundamentalMonads

object FiveMonads {

  trait Monad[M[_]] {
    def unit[A](a: A): M[A]
    def bind[A, B](ma: M[A])(amb: A => M[B]): M[B]
  }

  trait Monoid[A] {
    def mempty: A
    def mappend(x: A)(y: A): A
  }


  case class Identity[A](a: A)
  object Identity {
    implicit val identityMonad: Monad[Identity] = new Monad[Identity] {
      def unit[A](a: A): Identity[A] = Identity(a)
      def bind[A, B](ma: Identity[A])(amb: A => Identity[B]): Identity[B] =
        amb(ma.a)
    }
  }

  sealed trait Maybe[+A]
  case class Just[A](a: A) extends Maybe[A]
  case object None extends Maybe[Nothing]
  object Maybe {
    implicit val maybeMonad: Monad[Maybe] = new Monad[Maybe] {
      def unit[A](a: A): Maybe[A] = Just(a)
      def bind[A, B](ma: Maybe[A])(amb: A => Maybe[B]): Maybe[B] =  ma match {
        case Just(a) => amb(a)
        case _ => None
      }
    }
  }

  case class State[S, A](run: S => (S, A))
  object State {
    // inspired by https://stackoverflow.com/a/6248296
    implicit def stateMonad[S]: Monad[({
      type x[a] = State[S, a]
    })#x] = new Monad[({type x[a]=State[S, a]})#x] {
      def unit[A](a: A): State[S, A] = State((s: S) => (s,a))
      def bind[A, B](ma: State[S, A])(amb: A => State[S, B]): State[S, B] =
        State( (s: S) => {
          val (ss,a) = ma.run(s)
          amb(a).run(ss)
        })
    }
  }

  case class Reader[R, A](run: R => A)
  object Reader {
    implicit def readerMonad[R]: Monad[({
      type x[a] = Reader[R, a]
    })#x] = new Monad[({type x[a]=Reader[R, a]})#x] {
      def unit[A](a: A): Reader[R, A] = Reader(_ => a)
      def bind[A, B](ma: Reader[R, A])(amb: A => Reader[R, B]): Reader[R, B] =
        Reader(r => amb(ma.run(r)).run(r))
    }
  }

  case class Writer[W, A](run: (W, A))
  object Writer {
    implicit def readerMonad[W](implicit m: Monoid[W]): Monad[({
      type x[a] = Writer[W, a]
    })#x] = new Monad[({type x[a]=Writer[W, a]})#x] {
      def unit[A](a: A): Writer[W, A] = Writer(m.mempty,a)
      def bind[A, B](ma: Writer[W, A])(amb: A => Writer[W, B]): Writer[W, B] =
          Writer({
            val (w1,a) = ma.run
            val (w2,b) = amb(a).run
            (m.mappend(w1)(w2), b)
          })
    }
  }
}
