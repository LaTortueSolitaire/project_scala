import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)
    val uid = bank.generateAccountId

    def withdraw(amount: Double): Unit = amount match {
      case amount if amount <= 0 => throw new IllegalAmountException("Amount must be larger than zero.")
      case amount if getBalanceAmount < amount => throw new NoSufficientFundsException("Not enough funds available.")
      case _ => this.balance.amount = this.balance.amount - amount
    }
    def deposit(amount: Double): Unit = amount match {
      case amount if amount <= 0 => throw new IllegalAmountException("Amount must be larger than zero.")
      case _ => this.balance.amount = this.balance.amount + amount
    }
    def getBalanceAmount: Double = this.balance.amount

    def transferTo(account: Account, amount: Double) = {
        bank addTransactionToQueue (this, account, amount)
    }


}
