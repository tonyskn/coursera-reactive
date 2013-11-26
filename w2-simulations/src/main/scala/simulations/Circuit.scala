package simulations

import common._
import scala.language.postfixOps

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def transfer(input: Wire, output: Wire) {
    def transferAction() {
      val inputSig = input.getSignal
      afterDelay(0) { output.setSignal(inputSig) }
    }
    input addAction transferAction
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val a1i, a2i, a1i_and_a2i = new Wire
    inverter(a1, a1i)
    inverter(a2, a2i)
    andGate(a1i, a2i, a1i_and_a2i)
    inverter(a1i_and_a2i, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    (c, out) match {
      case (List(), List(out0)) => transfer(in, out0)
      case (List(c0), List(out1, out0)) => {
        val notC0 = new Wire
        inverter(c0, notC0)
        andGate(in, notC0, out0)
        andGate(in, c0   , out1)
      }
      case (cn :: cs, _) => {
        val (firstHalf, secondHalf) = out.splitAt(out.size / 2);
        val stage0, stage1 = new Wire
        demux(in, List(cn), List(stage1, stage0))
        demux(stage1, cs, firstHalf)
        demux(stage0, cs, secondHalf)
      }
    }
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  def orGateExample {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  def orGate2Example {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  def dmuxExample {
    val in = new Wire
    val c = for { i <- 0 until 4 toList } yield new Wire
    val out = for { i <- 0 until 16 toList } yield new Wire
    demux(in, c, out)
    probe("in", in)

    for ((_c, i) <- c.zipWithIndex) probe("c"+(c.size-1-i), _c)
    for ((_o, i) <- out.zipWithIndex) probe("o"+(out.size-1-i), _o)

    run

    in.setSignal(true); run
    c.head.setSignal(true); run
    c.head.setSignal(false); c.last.setSignal(true); run
  }
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
  Circuit.orGateExample
  Circuit.orGate2Example
  Circuit.dmuxExample
}
