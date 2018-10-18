
import scala.concurrent._

class Bank(val allowedAttempts: Integer = 3) {

    private val uid = new {
      var latestId = 0
    }
    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()
    private val executorContext = new forkjoin.ForkJoinPool

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit =  synchronized {
      transactionsQueue.push(new Transaction(transactionsQueue, processedTransactions, from, to, amount, allowedAttempts))
      executorContext.execute(new Runnable{
        def run() = processTransactions
      })
    }

    // Hint: use a counter
    def generateAccountId: Int = synchronized {
      uid.latestId += 1
      uid.latestId
    }

    private def processTransactions: Unit = synchronized {

      while ( ! this.transactionsQueue.isEmpty )  {
        val transaction : Transaction = this.transactionsQueue.pop
        this.processedTransactions.push( transaction )

        transaction.run

        if( transaction.status == TransactionStatus.FAILED && transaction.attempts<allowedAttempts-1){
          this.processedTransactions.pop
          transactionsQueue.push(transaction)
        }
      }
    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

}
