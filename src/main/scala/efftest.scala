package efftest
import cats.MonadThrow
import cats.data.Reader
import cats.data.Writer
import cats.syntax.all.*
import effect.*
import effect.TaskEffect.*
import effect.TaskInterpretation.*
import gears.async.Async
import org.atnos.eff.*
import org.atnos.eff.all.*
import org.atnos.eff.syntax.all.*

import java.time.Instant

case class Error(msg: String)

case class PropertyApiUrl(get: String)

case class PropertyId(get: Int)

case class Property(desc: String, id: PropertyId)

case class UserId(get: Int)

case class User(name: String, id: UserId, propertyId: PropertyId)

type _either[R] = MemberIn[Either[Error, *], R]
type _readerUrl[R] = MemberIn[Reader[PropertyApiUrl, *], R]

object UserRepository:

  def get[F[_]: MonadThrow](id: UserId): F[Either[Error, User]] =
    if id.get > 1000 then Right(User("Bob", id, PropertyId(123))).pure
    else Left(Error(s"Id ${id.get} in invalid range")).pure

def getUser[R: _either: _task](id: UserId): Eff[R, User] =
  for
    errorOrUser <- fromTask(UserRepository.get(id))
    user <- fromEither(errorOrUser)
  yield user

def getProperty[R: _either: _readerUrl](id: PropertyId): Eff[R, Property] =
  for
    propertyApiUrl <- ask[R, PropertyApiUrl]
    property <-
      if propertyApiUrl.get == "https://production.property-api.com" then right(Property("Big house!", id))
      else left(Error("Wrong URL!"))
  yield property

type _logger[R] = MemberIn[Writer[String, *], R]
type _readClock[R] = MemberIn[Reader[Instant, *], R]

def logTime[R: _logger: _readClock](): Eff[R, Unit] =
  for
    time <- ask[R, Instant]
    _ <- tell(s"The current time is $time")
  yield ()

def getPropertyForUserId(id: UserId): Task[Either[Error, Property]] =

  type AppStack = Fx.fx5[Reader[Instant, *], Either[Error, *], Writer[String, *], Reader[PropertyApiUrl, *], Task]

  val program: Eff[AppStack, Property] = for
    user <- getUser[AppStack](id)
    property <- getProperty[AppStack](user.propertyId)
    _ <- logTime[AppStack]() // Call our new function
  yield property

  val effTask: Eff[Fx.fx1[Task], Either[Error, Property]] = program
    .runReader(PropertyApiUrl("https://production.property-api.com"))
    .runEither
    .runReader(Instant.now())
    .runWriterUnsafe[String] { case log =>
      println(log) // print log message to stdout
    }
  effTask.toTask.flatTap {
    case Left(e)  => Task(println(e.msg)) // log errors
    case Right(p) => Task(println(s"User ${id.get} owns Property ${p.id.get}"))
  }

  // val result: Either[Error, Property] = TaskInterpretation.unsafeRunSync(effFuture)

  // result match
  //   case Left(e)  => println(e.msg) // log errors
  //   case Right(p) => println(s"User ${id.get} owns Property ${p.id.get}")

  // result
end getPropertyForUserId

// object Main extends IOApp:
//   def pureMain(args: List[String]): IO[Unit] =
//     getPropertyForUserId(UserId(1200)).asIO.void
