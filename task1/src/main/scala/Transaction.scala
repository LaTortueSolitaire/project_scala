import exceptions._
import scala.collection.mutable._

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

    var q = new Queue[Transaction]()

    // Remove and return the first element from the queue
    def pop: Transaction = {
        q.dequeue()
    }

    // Return whether the queue is empty
    def isEmpty: Boolean = {
        q.isEmpty
    }

    // Add new element to the back of the queue
    def push(t: Transaction): Unit = {
        q.enqueue(t)
    }

    // Return the first element from the queue without removing it
    def peek: Transaction = {
        q.front
    }

    // Return an iterator to allow you to iterate over the queue
    def iterator: Iterator[Transaction] = {
        q.iterator
    }
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

  var status: TransactionStatus.Value= TransactionStatus.PENDING
  var attempts = 0

  override def run: Unit = {

      def doTransaction() = {
        try {
          // println("j'essaye "+from.getBalanceAmount+" vers "+to.getBalanceAmount+" de "+amount)
          from withdraw amount
          to deposit amount
          this.status = TransactionStatus.SUCCESS
        } catch {
          case _ => this.status = TransactionStatus.FAILED
        }
        attempts = attempts+1
      }

      if (from.uid < to.uid) from synchronized {
          to synchronized {
            doTransaction
          }
      } else to synchronized {
          from synchronized {
            doTransaction
          }
      }
    }
}
