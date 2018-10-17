import scala.concurrent.forkjoin.ForkJoinPool

class Bank(val allowedAttempts: Integer = 3) {

    private val uid = new {
      var latestId = 0
    }
    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()
    //private val executorContext = "somethin"

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
      transactionsQueue.push(new Transaction(transactionsQueue, processedTransactions, from, to, amount, allowedAttempts))
      processTransactions
    }

    // Hint: use a counter
    def generateAccountId: Int = synchronized {
      uid.latestId += 1
      uid.latestId
    }

    private def processTransactions: Unit = {
      //while ( !this.transactionsQueue.isEmpty ) {
      //  var trans : Transaction = this.transactionsQueue.pop
      //  this.processedTransactions.push( trans )
      //  var t : Thread = new Thread( trans )
      //  t.start()
      //}
      while ( ! this.transactionsQueue.isEmpty )  {
        val transaction : Transaction = this.transactionsQueue.pop
        //val thread : Thread = new Thread( transaction )
        //thread.start
        // var attempt : Int =
        transaction.run
        this.processedTransactions.push( transaction )
      }
    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

}
