package com.my.dor_metagraph.l0.rewards.validators

import cats.effect.Async
import cats.effect.std.Env
import cats.syntax.all._
import com.my.dor_metagraph.shared_data.Utils.getDagAddressFromPublicKey
import io.constellationnetwork.schema.address.Address
import io.constellationnetwork.security.SecurityProvider

import scala.io.Source

trait ValidatorNodes[F[_]] {
  def getValidatorNodes: F[(List[Address], List[Address])]
}

object ValidatorNodes {

  /**
    * Validator-tax recipients are the committed per-network seedlists — the DETERMINISTIC source of
    * truth — read from the JAR classpath (no network calls, no per-node view). Per layer:
    *   - L0 metagraph nodes: `/ml0-<network>-seedlist`
    *   - data-L1 nodes:      `/dl1-<network>-seedlist`
    * Each line is a node public key (hex), converted to its DAG reward address. Every node on a
    * network ships the same seedlist files and resolves the same `CL_APP_ENV`, so all nodes derive
    * an identical recipient set -> identical reward transactions (no fork). This replaces the former
    * live `/cluster/info` HTTP query, whose per-node, time-varying view broke reward consensus.
    *
    * To add/remove a validator: edit the seedlist file and ship a coordinated release.
    */
  def fromSeedlist[F[_] : Async : SecurityProvider : Env]: ValidatorNodes[F] =
    new ValidatorNodes[F] {
      override def getValidatorNodes: F[(List[Address], List[Address])] =
        for {
          network <- resolveNetwork
          l0Addresses <- loadSeedlistAddresses(s"/ml0-$network-seedlist")
          l1Addresses <- loadSeedlistAddresses(s"/dl1-$network-seedlist")
        } yield (l0Addresses, l1Addresses)
    }

  // The network is fixed per deployment (all nodes on a network share it), so this selection is
  // deterministic across the fleet. Fail fast on an unset/unknown value rather than risk paying the
  // wrong network's validators.
  private[validators] def resolveNetwork[F[_] : Async : Env]: F[String] =
    Env[F].get("CL_APP_ENV").flatMap {
      case Some(value) =>
        value.trim.toLowerCase match {
          case "mainnet"        => "mainnet".pure[F]
          case "testnet"        => "testnet".pure[F]
          case "integrationnet" => "integrationnet".pure[F]
          case "dev"            => "testnet".pure[F] // dev mirrors the testnet seedlist
          case other =>
            Async[F].raiseError(new RuntimeException(s"Unknown CL_APP_ENV='$other'; cannot select validator seedlist"))
        }
      case None =>
        Async[F].raiseError(new RuntimeException("CL_APP_ENV is not set; cannot select validator seedlist"))
    }

  private def loadSeedlistAddresses[F[_] : Async : SecurityProvider](
    resourcePath: String
  ): F[List[Address]] =
    for {
      lines <- Async[F].blocking {
        val stream = Option(getClass.getResourceAsStream(resourcePath))
          .getOrElse(throw new RuntimeException(s"Validator seedlist resource not found on classpath: $resourcePath"))
        try Source.fromInputStream(stream).getLines().toList
        finally stream.close()
      }
      // First whitespace/comma-separated token of each non-blank line is the node public key (hex).
      publicKeys = lines.map(_.trim).filter(_.nonEmpty).map(_.split("[,\\s]+", 2).head)
      addresses <- publicKeys.traverse(getDagAddressFromPublicKey[F])
    } yield addresses
}
