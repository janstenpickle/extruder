package extruder.aws

import cats.Id
import cats.data.OptionT
import com.amazonaws.auth.{AWSCredentials, AWSCredentialsProvider, BasicAWSCredentials}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.scalacheck.any._
import extruder.aws.AwsCredentialsInstances._
import extruder.aws.credentials._
import extruder.core.{MultiParser, MultiShow}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{Assertion, EitherValues}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class AwsCredentialsInstancesSuite extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with EitherValues {
  import AwsCredentialsInstancesSuite._

  test("Parses valid ID and Key values")(passes)
  test("Returns none when no data is available")(isNoneNoData)
  test("Returns none when only the ID is available")(isNoneIdData)
  test("Returns none when only the key is available")(isNoneSecretData)
  test("Fails to parse invalid data")(fails)
  test("Shows credentials")(shows)

  def passes = {
    testPasses[AWSCredentials](creds => creds.getAWSAccessKeyId == validId && creds.getAWSSecretKey == validSecret)
    testPasses[AWSCredentialsProvider](
      creds => creds.getCredentials.getAWSAccessKeyId == validId && creds.getCredentials.getAWSSecretKey == validSecret
    )
  }

  def testPasses[A](test: A => Boolean)(implicit parser: MultiParser[Id, A]): Assertion =
    assert(
      test(
        parser
          .parse(path => OptionT.fromOption(validData.get(path)))
          .value
          .get
          .toEither
          .right
          .value
      )
    )

  def isNoneNoData =
    assert(
      MultiParser[Id, AWSCredentials]
        .parse(_ => OptionT.none)
        .value
        .isEmpty
    )

  def isNoneIdData =
    assert(
      MultiParser[Id, AWSCredentials]
        .parse(path => OptionT.fromOption(Map(List(AwsId) -> validId).get(path)))
        .value
        .isEmpty
    )

  def isNoneSecretData =
    assert(
      MultiParser[Id, AWSCredentials]
        .parse(path => OptionT.fromOption(Map(List(AwsSecret) -> validSecret).get(path)))
        .value
        .isEmpty
    )

  def fails = {
    testFails[AWSCredentials]
    testFails[AWSCredentialsProvider]
  }

  def testFails[A](implicit parser: MultiParser[Id, A]): Assertion = forAll {
    (id: String, secret: String Refined Not[SizeIs[_40]]) =>
      val data = Map(List(AwsId) -> id, List(AwsSecret) -> secret.value)
      assert(parser.parse(path => OptionT.fromOption(data.get(path))).value.get.toEither.left.value.size == 2)
  }

  def shows: Assertion = forAll { (id: String, sec: String) =>
    val res = MultiShow[AWSCredentials].show(new BasicAWSCredentials(id, sec))
    assert(res(List(AwsId)) === id)
    assert(res(List(AwsSecret)) === sec)
  }

}

object AwsCredentialsInstancesSuite {
  val validId = "AKIAIOSFODNN7EXAMPLE"
  val validSecret = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
  val validData = Map(List(AwsId) -> validId, List(AwsSecret) -> validSecret)
}
