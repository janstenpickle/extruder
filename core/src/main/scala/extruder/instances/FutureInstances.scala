package extruder.instances

trait FutureInstances {
//  def futureEffect(implicit ec: ExecutionContext): ExtruderEffect[Future] = new FromMonadError[Future]() {
//    override def runAsync[A](fa: Future[A])(cb: Either[Throwable, A] => IO[Unit]): IO[Unit] = ???
//
//    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Future[A] = {
//      val k0: (Try[A] => Unit) => Unit = ???
//
//      k0.
//    }
//
//    override def suspend[A](thunk: => Future[A]): Future[A] = thunk
//  }
}
