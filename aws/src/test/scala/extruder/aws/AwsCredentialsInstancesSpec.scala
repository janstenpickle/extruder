package extruder.aws

import cats.Id
import cats.data.{NonEmptyList, OptionT}
import com.amazonaws.auth.{AWSCredentials, BasicAWSCredentials}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.scalacheck.any._
import extruder.aws.AwsCredentialsInstances._
import extruder.aws.credentials._
import extruder.core.{MultiParser, MultiShow}
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

  def passes: MatchResult[Either[NonEmptyList[String], AWSCredentials]] =
    MultiParser[AWSCredentials]
      .parse[Id](path => OptionT.fromOption(validData.get(path)))
      .value
      .get
      .toEither must beRight.which { creds =>
      creds.getAWSAccessKeyId === validId && creds.getAWSSecretKey === validSecret
    }

  def isNoneNoData =
    MultiParser[AWSCredentials]
      .parse[Id](_ => OptionT.none)
      .value must beNone

  def isNoneIdData =
    MultiParser[AWSCredentials]
      .parse[Id](path => OptionT.fromOption(Map(List(AwsId) -> validId).get(path)))
      .value must beNone

  def isNoneSecretData =
    MultiParser[AWSCredentials]
      .parse[Id](path => OptionT.fromOption(Map(List(AwsSecret) -> validSecret).get(path)))
      .value must beNone

  def fails: Prop = prop { (id: String, secret: String Refined Not[SizeIs[_40]]) =>
    val data = Map(List(AwsId) -> id, List(AwsSecret) -> secret.value)
    MultiParser[AWSCredentials].parse[Id](path => OptionT.fromOption(data.get(path))).value.get.toEither must beLeft
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
}
