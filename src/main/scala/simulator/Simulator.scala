package simulator

import com.typesafe.scalalogging.LazyLogging
import ledger.Ledger
import stablecoin._

import scala.util.{Failure, Success}


class Simulator(initLedger: Ledger, env: Environment, players: Iterable[Player], duration: Int) extends LazyLogging {

  private var ledger = initLedger

  def run(): Unit = {
    for (i <- (0 until duration)) {
      logger.info("Starting round " + i)
      ledger = env.newRoundCallback(ledger, i)
      logger.info(ledger.stablecoinContract.toString)

      val transactions = players.flatMap(_.newRoundCallback(ledger, i))
      transactions.foreach { tx =>
        ledger.addTransaction(tx) match {
          case Success(_) => logger.trace("Transaction applied: " + tx.toString)
          case Failure(_) => logger.warn("Transaction rejected: " + tx.toString)
        }
      }

      logger.info("Ending round " + i)
      logger.info(ledger.stablecoinContract.toString)
    }

    logger.info("End of simulation. " + ledger.stablecoinContract.toString)
  }
}
