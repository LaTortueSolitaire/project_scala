import akka.actor._
import exceptions._
import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()

class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

    private var transactions = HashMap[String, Transaction]()

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)

    def getFullAddress: String = {
        bankId + accountId
    }

    def getTransactions: List[Transaction] = {
        // Should return a list of all Transaction-objects stored in transactions
        var trans : List = new List()
        for ( ( k, t ) <- this.transactions t :: trans  ) ) 
    }

    def allTransactionsCompleted: Boolean = {
        // Should return whether all Transaction-objects in transactions are completed
        for ( ( k, t ) <- this.transactions if( !t.isCompleted ) { false } )
        true
    }

    def withdraw(amount: Double): Unit = amount match {
      case amount if amount <= 0 => throw new IllegalAmountException("Amount must be larger than zero.")
      case amount if getBalanceAmount < amount => throw new NoSufficientFundsException("Not enough funds available.")
      case _ => synchronized { this.balance.amount = getBalanceAmount - amount }
    }

    def deposit(amount: Double): Unit = amount match {
      case amount if amount <= 0 => throw new IllegalAmountException("Amount must be larger than zero.")
      case _ => synchronized { this.balance.amount = getBalanceAmount + amount }
    }

    def getBalanceAmount: Double = synchronized { this.balance.amount }

    def sendTransactionToBank(t: Transaction): Unit = {
        // Should send a message containing t to the bank of this account
        BankManager.findBank( this.bankId ) ! t
    }

    def transferTo(accountNumber: String, amount: Double): Transaction = {

        val t = new Transaction(from = getFullAddress, to = accountNumber, amount = amount)

        if (reserveTransaction(t)) {
            try {
                withdraw(amount)
                sendTransactionToBank(t)

            } catch {
                case _: NoSufficientFundsException | _: IllegalAmountException =>
                    t.status = TransactionStatus.FAILED
            }
        }

        t

    }

    def reserveTransaction(t: Transaction): Boolean = {
      if (!transactions.contains(t.id)) {
        transactions += (t.id -> t)
        return true
      }
      false
    }

    override def receive = {
		case IdentifyActor => sender ! this

		case TransactionRequestReceipt(to, transactionId, transaction) => {
			// Process receipt
            ???
		}

		case BalanceRequest => ??? // Should return current balance

		case t: Transaction => {
			// Handle incoming transaction
			???
		}

		case msg => ???
    }


}
