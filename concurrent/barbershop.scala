/**
 * This is my Scala solution to the sleeping barber problem
 * (http://en.wikipedia.org/wiki/Sleeping_barber_problem).
 * The problem here is a little different in that multiple barbers are
 * allowed, and they actually get all the customers shaved faster too.
 *
 * We create one actor per customer, one actor for each barber, and
 * one actor for a shop.  The shop owns a queue of customers waiting
 * for a shave, and barbers looking for work.  The customer asks the
 * shop if a seat is available.  If there's a seat, the customer is added
 * to the queue, otherwise the shop lets the customer know to try again
 * later.  The barber tells the shop he's looking for work.  If there's
 * a customer waiting, the barber is asked to shave the customer.
 * Otherwise, the barber is added to a queue.  When a customer walks in,
 * the barber is removed from the queue and asked to shave the customer.
 * Once the barber has shaved a number of customers equal to the total
 * number of customers, all the actors shutdown.  The simulation then
 * verifies that all customers are shaved.
 */

import scala.actors.Actor
import scala.collection.mutable.Queue

val totalSeats = 4
val totalCustomers = 1000

// Messages
case class LookingForWork(barber: Actor)
case class TakeASeat(cust: Actor)
case class PleaseShave(cust: Actor)
case object SorryTryAgain
case object Shaved
case object Quit

// Represents queues of customers and barbers.
class Shop extends Actor {
  private val customers = Queue[Actor]()
  private val barbers   = Queue[Actor]()
  def act {
    loop {
      react {
	case TakeASeat(cust) =>
	  if (!barbers.isEmpty) {
	    barbers.dequeue() ! PleaseShave(cust)
	  } else {
	    if (customers.size < totalSeats) {
	      customers += cust
	    } else {
	      cust ! SorryTryAgain
	    }
	  }

	case LookingForWork(barber) =>
	  if (!customers.isEmpty) {
	    barber ! PleaseShave(customers.dequeue())
	  } else {
	    barbers += barber
	  }
	
	case Quit =>
	  exit()
      }
    }
  }
}

// A customer's face is either 'hairy or 'shaved.
// The customer tries to take a seat in the barber shop.
// If no seat is available the shop will tell him to try again.
class Customer(sim: Actor, shop: Actor) extends Actor {
  private var face = 'hairy
  def getFace = face
  def act {
    shop ! TakeASeat(this)
    react {
      case SorryTryAgain =>
	act
      case Shaved =>
	face = 'shaved
        sim ! Shaved
    }
  }
}

// The barber tells the shop that he's looking for work.
// If there's a customer in the shop, the shop will ask
// the barber to please shave the customer.
class Barber(sim: Actor, shop: Actor) extends Actor {
  def act {
    loop {
      shop ! LookingForWork(this)
      react {
	case PleaseShave(cust) =>
	  cust ! Shaved
	case Quit =>
	  exit()
      }
    }
  }
}

// The simulation owns all the actors and starts them up.
// For each person shaved, the Sim is notified.  Once the number of shaves
// is equal to the number of customers, the Sim verifies that all customers
// are 'shaved.
class Sim extends Actor {
  private var shaves = 0
  private val shop = new Shop
  private val barber1 = new Barber(this, shop)
  private val barber2 = new Barber(this, shop)
  private val customers =
      1 to totalCustomers map { _ => new Customer(this, shop) }
  shop.start()
  barber1.start()
  barber2.start()
  customers foreach (cust => cust.start())

  def act {
    loop {
      react {
	case Shaved =>
	  shaves += 1
	  println("Shave #" + shaves)
          if (shaves == totalCustomers) {
	    // Verify that everyone is shaved
	    if (customers forall { cust => cust.getFace == 'shaved }) {
	      println("Every one is bald!")
	    } else {
	      println("Something went wrong!")
	    }
	    // Shut everyone down.
	    barber1 ! Quit
	    barber2 ! Quit
	    shop ! Quit
	    exit()
	  }
      }
    }
  }
}

val sim = new Sim
sim.start()
