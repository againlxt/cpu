package baseexu

import chisel3._
import chisel3.util._
import gate._

class FullAdder extends Module {
	val io = IO(new Bundle {
		val a 		= Input(UInt(1.W))
		val b 		= Input(UInt(1.W))
		val cin 	= Input(UInt(1.W))
		val s 		= Output(UInt(1.W))
		val p 	 	= Output(UInt(1.W))
		val g 		= Output(UInt(1.W))
	})

	io.s := io.a ^ io.b ^ io.cin
	io.p := io.a | io.b
	io.g := io.a & io.b
}

class CLA4 extends Module {
  val io = IO(new Bundle {
    val a    = Input(UInt(4.W))
    val b    = Input(UInt(4.W))
    val cin  = Input(UInt(1.W))
    val cout = Output(UInt(1.W))
    val sum  = Output(UInt(4.W))
    val PG   = Output(UInt(1.W))
    val GG   = Output(UInt(1.W))
  })

  val adder0 = Module(new FullAdder)
  val adder1 = Module(new FullAdder)
  val adder2 = Module(new FullAdder)
  val adder3 = Module(new FullAdder)

  val s0 = Wire(UInt(1.W))
  val s1 = Wire(UInt(1.W))
  val s2 = Wire(UInt(1.W))
  val s3 = Wire(UInt(1.W))

  val p0 = Wire(UInt(1.W))
  val p1 = Wire(UInt(1.W))
  val p2 = Wire(UInt(1.W))
  val p3 = Wire(UInt(1.W))

  val g0 = Wire(UInt(1.W))
  val g1 = Wire(UInt(1.W))
  val g2 = Wire(UInt(1.W))
  val g3 = Wire(UInt(1.W))

  val c1 = Wire(UInt(1.W))
  val c2 = Wire(UInt(1.W))
  val c3 = Wire(UInt(1.W))
  val c4 = Wire(UInt(1.W))

  // Connect FullAdder instances
  adder0.io.a := io.a(0)
  adder0.io.b := io.b(0)
  adder0.io.cin := io.cin
  s0 := adder0.io.s
  p0 := adder0.io.p
  g0 := adder0.io.g
  c1 := g0 | (p0 & io.cin)

  adder1.io.a := io.a(1)
  adder1.io.b := io.b(1)
  adder1.io.cin := c1
  s1 := adder1.io.s
  p1 := adder1.io.p
  g1 := adder1.io.g
  c2 := g1 | (p1 & c1)

  adder2.io.a := io.a(2)
  adder2.io.b := io.b(2)
  adder2.io.cin := c2
  s2 := adder2.io.s
  p2 := adder2.io.p
  g2 := adder2.io.g
  c3 := g2 | (p2 & c2)

  adder3.io.a := io.a(3)
  adder3.io.b := io.b(3)
  adder3.io.cin := c3
  s3 := adder3.io.s
  p3 := adder3.io.p
  g3 := adder3.io.g
  c4 := g3 | (p3 & c3)

  io.cout := c4
  io.sum := Cat(s3, s2, s1, s0)
  io.PG := p0 & p1 & p2 & p3
  io.GG := g3 | (g2 & p3) | (g1 & p3 & p2) | (g0 & p3 & p2 & p1)
}

class CLAGen(val n: Int) extends Module {
  val io = IO(new Bundle {
    val a    = Input(UInt(n.W))
    val b    = Input(UInt(n.W))
    val cin  = Input(UInt(1.W))
    val cout = Output(UInt(1.W))
    val sum  = Output(UInt(n.W))
    val PG   = Output(UInt(1.W))
    val GG   = Output(UInt(1.W))
  })

  val adders = VecInit(Seq.fill(n)(Module(new FullAdder).io))
  
  val sums = Wire(Vec(n, UInt(1.W)))
  val ps = Wire(Vec(n, UInt(1.W)))
  val gs = Wire(Vec(n, UInt(1.W)))
  val cs = Wire(Vec(n + 1, UInt(1.W)))

  cs(0) := io.cin

  for (i <- 0 until n) {
    adders(i).a := io.a(i)
    adders(i).b := io.b(i)
    adders(i).cin := cs(i)
    sums(i) := adders(i).s
    ps(i) := adders(i).p
    gs(i) := adders(i).g
    cs(i + 1) := gs(i) | (ps(i) & cs(i))
  }

  io.sum := sums.asUInt
  io.cout := cs(n)
  io.PG := ps.reduce(_ & _)
  io.GG := (0 until n).map(i => gs(i) & (if (i > 0) ps.take(i).reduce(_ & _) else 1.U)).reduce(_ | _)
}
