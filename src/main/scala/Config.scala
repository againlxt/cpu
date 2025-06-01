package cpu

import chisel3._
import chisel3.util._

object Config {
  def hasPerformanceCounter:  Boolean = true
  def SoC:                    Boolean = true
}
