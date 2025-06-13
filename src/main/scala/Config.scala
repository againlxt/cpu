package cpu

import chisel3._
import chisel3.util._

object Config {
  def hasPerformanceCounter:  Boolean = true
  def hasDPIC:                Boolean = false
  def isSTA:                  Boolean = false
  def SoC:                    Boolean = true
}
