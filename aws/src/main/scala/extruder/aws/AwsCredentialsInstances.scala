package extruder.aws

import cats.Monad
import cats.data.{OptionT, ValidatedNel}
import cats.syntax.either._
import com.amazonaws.auth.{AWSCredentials, AWSCredentialsProvider, BasicAWSCredentials}
import eu.timepit.refined._
import eu.timepit.refined.boolean.And
import eu.timepit.refined.collection.Size
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.string.MatchesRegex
import extruder.core.{MultiParser, MultiShow}

trait AwsCredentialsInstances {
  import AwsCredentialsInstances._

  implicit def credentialsParser[F[_]: Monad]: MultiParser[F, AWSCredentials] =
    new MultiParser[F, AWSCredentials] {
      override def parse(lookup: List[String] => OptionT[F, String]): OptionT[F, ValidatedNel[String, AWSCredentials]] =
        for {
          id <- lookup(List(AwsId))
          secret <- lookup(List(AwsSecret))
        } yield
          refineV[AwsId](id).toValidatedNel.product(refineV[AwsSecret](secret).toValidatedNel).map {
            case (i, s) => new BasicAWSCredentials(i.value, s.value)
          }
    }

  implicit def credentialsProviderParser[F[_]: Monad]: MultiParser[F, AWSCredentialsProvider] =
    credentialsParser.map(
      credentials =>
        new AWSCredentialsProvider {
          override def getCredentials: AWSCredentials = credentials
          override def refresh(): Unit = ()
      }
    )

  implicit val credentialsShow: MultiShow[AWSCredentials] = new MultiShow[AWSCredentials] {
    override def show(v: AWSCredentials): Map[List[String], String] =
      Map(List(AwsId) -> v.getAWSAccessKeyId, List(AwsSecret) -> v.getAWSSecretKey)
  }

  implicit val credentialsProviderShow: MultiShow[AWSCredentialsProvider] = MultiShow.by(_.getCredentials)
}

object AwsCredentialsInstances {
  type _20 = W.`20`.T
  type _40 = W.`40`.T

  type SizeIs[N] = Size[Interval.Closed[N, N]]
  type IdRegex = MatchesRegex[W.`"^[A-Z0-9]+$"`.T]

  type AwsId = And[SizeIs[_20], IdRegex]
  type AwsSecret = SizeIs[_40]

  val AwsId = "AwsAccessKeyId"
  val AwsSecret = "AwsSecretAccessKey"
}
