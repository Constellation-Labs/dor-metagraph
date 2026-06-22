package com.my.dor_metagraph.l0

import cats.effect.{IO, Resource}
import cats.syntax.all._
import com.my.dor_metagraph.shared_data.Utils.getDagAddressFromPublicKey
import io.constellationnetwork.security.SecurityProvider
import weaver.MutableIOSuite

import scala.io.Source

/**
  * Verifies the per-network validator seedlists are actually bundled on the L0 classpath (the build
  * wires them via unmanagedResourceDirectories) and that every line is a valid node public key that
  * converts to a DAG reward address. If a seedlist is missing/misbuilt or malformed, reward
  * distribution would fail at runtime — this catches that at build/test time instead.
  */
object SeedlistResourcesTest extends MutableIOSuite {

  override type Res = SecurityProvider[IO]
  override def sharedResource: Resource[IO, SecurityProvider[IO]] = SecurityProvider.forAsync[IO]

  private val seedlistResources = List(
    "ml0-mainnet-seedlist", "dl1-mainnet-seedlist",
    "ml0-testnet-seedlist", "dl1-testnet-seedlist",
    "ml0-integrationnet-seedlist", "dl1-integrationnet-seedlist"
  )

  seedlistResources.foreach { name =>
    test(s"seedlist resource $name is bundled and parses to valid DAG addresses") { implicit sp =>
      val publicKeys =
        Option(getClass.getResourceAsStream(s"/$name")) match {
          case None => Nil
          case Some(stream) =>
            try Source.fromInputStream(stream).getLines().toList.map(_.trim).filter(_.nonEmpty).map(_.split("[,\\s]+", 2).head)
            finally stream.close()
        }

      publicKeys.traverse(getDagAddressFromPublicKey[IO]).map { addresses =>
        expect(publicKeys.nonEmpty) &&
          expect.eql(publicKeys.size, addresses.size) &&
          expect(addresses.forall(_.value.value.startsWith("DAG")))
      }
    }
  }
}
