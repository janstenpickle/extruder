package extruder.aws

import cats.Id
import cats.data.{NonEmptyList, OptionT}
import com.amazonaws.auth.{AWSCredentials, AWSCredentialsProvider, BasicAWSCredentials}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.scalacheck.any._
import extruder.aws.AwsCredentialsInstances._
import extruder.aws.credentials._
import extruder.core.{MultiParser, MultiShow}
import extruder.effect.ExtruderMonadError
import org.scalacheck.Prop
import org.specs2.matcher.{EitherMatchers, MatchResult}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class AwsCredentialsInstancesSpec extends Specification with ScalaCheck with EitherMatchers {
  import AwsCredentialsInstancesSpec._

  override def is: SpecStructure =
    s2"""
       Parses valid ID and Key values $passes
       Returns none when no data is available $isNoneNoData
       Returns none when only the ID is available $isNoneIdData
       Returns none when only the key is available $isNoneSecretData
       Fails to parse invalid data $fails

       Shows credentials $shows
      """

  def passes =
    testPasses[AWSCredentials](creds => creds.getAWSAccessKeyId == validId && creds.getAWSSecretKey == validSecret) &&
      testPasses[AWSCredentialsProvider](
        creds =>
          creds.getCredentials.getAWSAccessKeyId == validId && creds.getCredentials.getAWSSecretKey == validSecret
      )

  def testPasses[A](
    test: A => Boolean
  )(implicit parser: MultiParser[Id, A]): MatchResult[Either[NonEmptyList[String], A]] =
    parser
      .parse(path => OptionT.fromOption(validData.get(path)))
      .value
      .get
      .toEither must beRight.which(test)

  def isNoneNoData =
    MultiParser[Id, AWSCredentials]
      .parse(_ => OptionT.none)
      .value must beNone

  def isNoneIdData =
    MultiParser[Id, AWSCredentials]
      .parse(path => OptionT.fromOption(Map(List(AwsId) -> validId).get(path)))
      .value must beNone

  def isNoneSecretData =
    MultiParser[Id, AWSCredentials]
      .parse(path => OptionT.fromOption(Map(List(AwsSecret) -> validSecret).get(path)))
      .value must beNone

  def fails = testFails[AWSCredentials] && testFails[AWSCredentialsProvider]

  def testFails[A](implicit parser: MultiParser[Id, A]): Prop = prop {
    (id: String, secret: String Refined Not[SizeIs[_40]]) =>
      val data = Map(List(AwsId) -> id, List(AwsSecret) -> secret.value)
      parser.parse(path => OptionT.fromOption(data.get(path))).value.get.toEither must beLeft
        .which(_.size == 2)
  }

  def shows: Prop = prop { (id: String, sec: String) =>
    val res = MultiShow[AWSCredentials].show(new BasicAWSCredentials(id, sec))
    res(List(AwsId)) === id && res(List(AwsSecret)) === sec
  }

}

object AwsCredentialsInstancesSpec {
  val validId = "AKIAIOSFODNN7EXAMPLE"
  val validSecret = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
  val validData = Map(List(AwsId) -> validId, List(AwsSecret) -> validSecret)

  implicit val idMonadError: ExtruderMonadError[Id] = new ExtruderMonadError[Id] {
    override def missing[A](message: String): Id[A] = ???
    override def validationFailure[A](message: String): Id[A] = ???
    override def validationException[A](message: String, ex: Throwable): Id[A] = ???
    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] =
      cats.catsInstancesForId.flatMap(fa)(f)
    override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] =
      cats.catsInstancesForId.tailRecM(a)(f)
    override def raiseError[A](e: Throwable): Id[A] = ???
    override def handleErrorWith[A](fa: Id[A])(f: Throwable => Id[A]): Id[A] = ???
    override def pure[A](x: A): Id[A] = cats.catsInstancesForId.pure(x)
  }
}
