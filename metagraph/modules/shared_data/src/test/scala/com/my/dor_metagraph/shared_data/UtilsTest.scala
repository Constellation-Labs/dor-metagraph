package com.my.dor_metagraph.shared_data

import cats.effect.{IO, Resource}
import com.my.dor_metagraph.shared_data.Utils.getDagAddressFromPublicKey
import org.tessellation.security.SecurityProvider
import weaver.MutableIOSuite

object UtilsTest extends MutableIOSuite {

  override type Res = SecurityProvider[IO]

  override def sharedResource: Resource[IO, SecurityProvider[IO]] = SecurityProvider.forAsync[IO]

  test("Test get DAG address from pub_key") { implicit sp: SecurityProvider[IO] =>
    val publicKey = "d741b547225b6ba6f1ba38be192ab7550b7610ef54e7fee88a9666b79a12a6741d1565241fba5c2a812be66edd878824f927a42430ffba48fa0bd0264a5483bf"
    for {
      address <- getDagAddressFromPublicKey(publicKey)
    } yield expect("DAG3Z6oMiqXyi4SKEU4u4fwNiYAMYFyPwR3ttTSd" == address.value.value)
  }

}